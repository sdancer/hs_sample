module X86Sem where

import Ast
import Hapstone.Capstone
import Hapstone.Internal.Capstone as Capstone
import Hapstone.Internal.X86      as X86
import Util
import AstContext
import Data.Maybe
import Data.Word
import BitVector

-- Makes an expression representing the address of the next instruction

next_instr_ptr :: [CsMode] -> CsInsn -> Expr

next_instr_ptr modes insn = BvExpr (bitVector (insnAddr + insnLen) archBitSize)
  where insnAddr = convert (address insn)
        insnLen = convert (length (bytes insn))
        archBitSize = convert (get_arch_bit_size modes)

-- Make operation to increase instruction pointer by size of instruction

inc_insn_ptr :: [CsMode] -> CsInsn -> Stmt

inc_insn_ptr modes insn = SetReg (get_insn_ptr modes) (next_instr_ptr modes insn)

-- Gets an expression for the given operand. Processor mode may be needed for computing
-- effective addresses.

getOperandAst :: [CsMode] -> CsX86Op -> Expr

getOperandAst modes op = case value op of
  (Imm value) -> BvExpr (bitVector (convert value) (convert (size op) * 8))
  (Reg reg) -> GetReg (fromX86Reg reg)
  (Mem mem) -> Load (convert $ size op) (getLeaAst modes mem)

-- Gets the memory structure for the given operand.

getMem :: CsX86OpValue -> X86OpMemStruct

getMem op_val = case op_val of
  (Imm value) -> error "Memory operand expected but immediate value found instead."
  (Reg reg) -> error "Memory operand expected but register value found instead."
  (Mem mem) -> mem

-- Gets the specified register after it has been zero extended to the architecture size

getZxRegister :: [CsMode] -> CompoundReg -> Expr

getZxRegister modes reg =
  ZxExpr (arch_bit_size - reg_size) (GetReg reg) where
    arch_bit_size = get_arch_bit_size modes
    reg_size = getRegSize reg

-- Gets an expression for the memory location of the given memory operand

getLeaAst :: [CsMode] -> X86OpMemStruct -> Expr

getLeaAst modes mem =
  (BvaddExpr node_disp (BvaddExpr node_base node_index)) where
    arch_bit_size = get_arch_bit_size modes
    node_base = case base mem of
      X86RegInvalid -> BvExpr (bitVector 0 arch_bit_size)
      reg -> getZxRegister modes (fromX86Reg reg)
    node_index = case index mem of
      X86RegInvalid -> BvExpr (bitVector 0 arch_bit_size)
      reg -> BvmulExpr (getZxRegister modes (fromX86Reg reg)) (BvExpr (bitVector (fromIntegral $ scale mem) arch_bit_size))
    node_disp = BvExpr (bitVector (fromIntegral $ disp' mem) arch_bit_size)

-- Make operation to store the given expression in the given operand

store_stmt :: [CsMode] -> CsX86Op -> Expr -> Stmt

store_stmt modes operand store_what =
  case (value operand) of
    (Reg reg) -> SetReg (fromX86Reg reg) store_what
    (Mem mem) -> Store (convert $ size operand) (getLeaAst modes mem) store_what
    _ -> error "Target of store operation is neither a register nor a memory operand."

-- Make operation to set the zero flag to the value that it would have after some operation

zf_s :: Expr -> CsX86Op -> Stmt

zf_s parent dst =
  let bv_size = (convert $ size dst) * 8 in
    SetReg (fromX86Flag X86FlagZf) (IteExpr
      (EqualExpr (ExtractExpr 0 bv_size parent) (BvExpr (bitVector 0 bv_size)))
      (BvExpr (bitVector 1 1))
      (BvExpr (bitVector 0 1)))

-- Make operation to set the overflow flag to the value that it would have after an add operation

of_add_s :: Expr -> CsX86Op -> Expr -> Expr -> Stmt

of_add_s parent dst op1ast op2ast =
  let bv_size = (convert $ size dst) * 8 in
    SetReg (fromX86Flag X86FlagOf) (ExtractExpr (bv_size - 1) bv_size
      (BvandExpr
        (BvxorExpr op1ast (BvnotExpr op2ast))
        (BvxorExpr op1ast (ExtractExpr 0 bv_size parent))))

-- Make operation to set the carry flag to the value that it would have after an add operation

cf_add_s :: Expr -> CsX86Op -> Expr -> Expr -> Stmt

cf_add_s parent dst op1ast op2ast =
  let bv_size = (convert $ size dst) * 8 in
    SetReg (fromX86Flag X86FlagCf) (ExtractExpr (bv_size - 1) bv_size
      (BvxorExpr (BvandExpr op1ast op2ast)
        (BvandExpr (BvxorExpr
            (BvxorExpr op1ast op2ast)
            (ExtractExpr 0 bv_size parent))
          (BvxorExpr op1ast op2ast))))

-- Make operation to set the adjust flag to the value that it would have after some operation

af_s :: Expr -> CsX86Op -> Expr -> Expr -> Stmt

af_s parent dst op1ast op2ast =
  let bv_size = (convert $ size dst) * 8 in
    SetReg (fromX86Flag X86FlagAf) (IteExpr
      (EqualExpr
        (BvExpr (bitVector 0x10 bv_size))
        (BvandExpr
          (BvExpr (bitVector 0x10 bv_size))
          (BvxorExpr
            (ExtractExpr 0 bv_size parent)
            (BvxorExpr op1ast op2ast))))
      (BvExpr (bitVector 1 1))
      (BvExpr (bitVector 0 1)))

-- Make operation to set the parity flag to the value that it would have after some operation

pf_s :: Expr -> CsX86Op -> Stmt

pf_s parent dst =
  let loop counter =
        (if counter == byte_size_bit
          then BvExpr (bitVector 1 1)
          else (BvxorExpr
            (loop (counter + 1))
            (ExtractExpr 0 1
              (BvlshrExpr
                (ExtractExpr 0 byte_size_bit parent)
                (BvExpr (bitVector counter byte_size_bit)))))) in
    SetReg (fromX86Flag X86FlagPf) (loop 0)

-- Make operation to set the sign flag to the value that it would have after some operation

sf_s :: Expr -> CsX86Op -> Stmt

sf_s parent dst =
  let bv_size = (convert $ size dst) * 8 in
    SetReg (fromX86Flag X86FlagSf) (ExtractExpr (bv_size - 1) bv_size parent)

-- Make list of operations in the IR that has the same semantics as the X86 add instruction

add_s :: [CsMode] -> CsInsn -> [Stmt]

add_s modes inst =
  let (dst : src : _ ) = x86operands inst
      dst_ast = getOperandAst modes dst
      src_ast = getOperandAst modes src
      add_node = (BvaddExpr dst_ast src_ast)
  in [
      inc_insn_ptr modes inst,
      store_stmt modes dst add_node,
      af_s add_node dst dst_ast src_ast,
      pf_s add_node dst,
      sf_s add_node dst,
      zf_s add_node dst,
      cf_add_s add_node dst dst_ast src_ast,
      of_add_s add_node dst dst_ast src_ast
    ]

-- Make list of operations in the IR that has the same semantics as the X86 inc instruction

inc_s :: [CsMode] -> CsInsn -> [Stmt]

inc_s modes inst =
  let (dst : _ ) = x86operands inst
      dst_ast = getOperandAst modes dst
      src_ast = BvExpr (bitVector 1 (fromIntegral (size dst * 8)))
      add_node = (BvaddExpr dst_ast src_ast)
  in [
      inc_insn_ptr modes inst,
      store_stmt modes dst add_node,
      af_s add_node dst dst_ast src_ast,
      pf_s add_node dst,
      sf_s add_node dst,
      zf_s add_node dst,
      of_add_s add_node dst dst_ast src_ast
    ]

-- Make operation to set the carry flag to the value that it would have after an sub operation

cf_sub_s :: Expr -> CsX86Op -> Expr -> Expr -> Stmt

cf_sub_s parent dst op1ast op2ast =
  let bv_size = (convert $ size dst) * 8 in
    SetReg (fromX86Flag X86FlagCf) (ExtractExpr (bv_size - 1) bv_size
      (BvxorExpr
        (BvxorExpr op1ast (BvxorExpr op2ast (ExtractExpr 0 bv_size parent)))
        (BvandExpr
          (BvxorExpr op1ast (ExtractExpr 0 bv_size parent))
          (BvxorExpr op1ast op2ast))))

-- Make operation to set the overflow flag to the value that it would have after an sub operation

of_sub_s :: Expr -> CsX86Op -> Expr -> Expr -> Stmt

of_sub_s parent dst op1ast op2ast =
  let bv_size = (convert $ size dst) * 8 in
    SetReg (fromX86Flag X86FlagOf) (ExtractExpr (bv_size - 1) bv_size
      (BvandExpr
        (BvxorExpr op1ast op2ast)
        (BvxorExpr op1ast (ExtractExpr 0 bv_size parent))))

-- Make list of operations in the IR that has the same semantics as the X86 sub instruction

sub_s :: [CsMode] -> CsInsn -> [Stmt]

sub_s modes inst =
  let (dst : src : _ ) = x86operands inst
      dst_ast = getOperandAst modes dst
      src_ast = getOperandAst modes src
      sub_node = (BvsubExpr dst_ast src_ast)
  in [
      inc_insn_ptr modes inst,
      store_stmt modes dst sub_node,
      af_s sub_node dst dst_ast src_ast,
      pf_s sub_node dst,
      sf_s sub_node dst,
      zf_s sub_node dst,
      cf_sub_s sub_node dst dst_ast src_ast,
      of_sub_s sub_node dst dst_ast src_ast
    ]

-- Make list of operations in the IR that has the same semantics as the X86 cmp instruction

cmp_s :: [CsMode] -> CsInsn -> [Stmt]

cmp_s modes inst =
  let (dst : src : _ ) = x86operands inst
      dst_ast = getOperandAst modes dst
      src_ast = SxExpr (convert ((size dst) - (size src)) * 8) (getOperandAst modes src)
      cmp_node = BvsubExpr dst_ast src_ast
  in [
      inc_insn_ptr modes inst,
      af_s cmp_node dst dst_ast src_ast,
      pf_s cmp_node dst,
      sf_s cmp_node dst,
      zf_s cmp_node dst,
      cf_sub_s cmp_node dst dst_ast src_ast,
      of_sub_s cmp_node dst dst_ast src_ast
    ]

-- Make list of operations in the IR that has the same semantics as the X86 xor instruction

xor_s :: [CsMode] -> CsInsn -> [Stmt]

xor_s modes inst =
  let (dst_op : src_op : _ ) = x86operands inst
      dst_ast = getOperandAst modes dst_op
      src_ast = getOperandAst modes src_op
      xor_node = (BvxorExpr dst_ast src_ast)
  in [
      inc_insn_ptr modes inst,
      store_stmt modes dst_op xor_node,
      SetReg (fromX86Flag X86FlagAf) UndefinedExpr,
      pf_s xor_node dst_op,
      sf_s xor_node dst_op,
      zf_s xor_node dst_op,
      SetReg (fromX86Flag X86FlagCf) (BvExpr (bitVector 0 1)),
      SetReg (fromX86Flag X86FlagOf) (BvExpr (bitVector 0 1))
    ]

-- Make list of operations in the IR that has the same semantics as the X86 and instruction

and_s :: [CsMode] -> CsInsn -> [Stmt]

and_s modes inst =
  let (dst_op : src_op : _ ) = x86operands inst
      dst_ast = getOperandAst modes dst_op
      src_ast = getOperandAst modes src_op
      and_node = (BvandExpr dst_ast src_ast)
  in [
      inc_insn_ptr modes inst,
      store_stmt modes dst_op and_node,
      SetReg (fromX86Flag X86FlagAf) UndefinedExpr,
      pf_s and_node dst_op,
      sf_s and_node dst_op,
      zf_s and_node dst_op,
      SetReg (fromX86Flag X86FlagCf) (BvExpr (bitVector 0 1)),
      SetReg (fromX86Flag X86FlagOf) (BvExpr (bitVector 0 1))
    ]

-- Make list of operations in the IR that has the same semantics as the X86 or instruction

or_s :: [CsMode] -> CsInsn -> [Stmt]

or_s modes inst =
  let (dst_op : src_op : _ ) = x86operands inst
      dst_ast = getOperandAst modes dst_op
      src_ast = getOperandAst modes src_op
      and_node = (BvorExpr dst_ast src_ast)
  in [
      inc_insn_ptr modes inst,
      store_stmt modes dst_op and_node,
      SetReg (fromX86Flag X86FlagAf) UndefinedExpr,
      pf_s and_node dst_op,
      sf_s and_node dst_op,
      zf_s and_node dst_op,
      SetReg (fromX86Flag X86FlagCf) (BvExpr (bitVector 0 1)),
      SetReg (fromX86Flag X86FlagOf) (BvExpr (bitVector 0 1))
    ]

-- Make list of operations in the IR that has the same semantics as the X86 push instruction

push_s :: [CsMode] -> CsInsn -> [Stmt]

push_s modes inst =
  let (src : _) = x86operands inst
      sp = (get_stack_reg modes)
      arch_byte_size = get_arch_byte_size modes
      -- If it's an immediate source, the memory access is always based on the arch size
      op_size = case (value src) of
        (Imm _) -> arch_byte_size
        _ -> convert $ size src
  in [
      inc_insn_ptr modes inst,
      SetReg sp (BvsubExpr (GetReg sp) (BvExpr (bitVector (convert op_size) (arch_byte_size * 8)))),
      Store op_size (GetReg sp) (ZxExpr ((op_size - (convert $ size src)) * 8) (getOperandAst modes src))
    ]

-- Makes a singleton list containing the argument if the condition is true. Otherwise makes
-- the empty list.

includeIf :: Bool -> [a] -> [a]

includeIf cond sublist = if cond then sublist else []

-- Make list of operations in the IR that has the same semantics as the X86 pop instruction

pop_s :: [CsMode] -> CsInsn -> [Stmt]

pop_s modes inst =
  let (dst : _) = x86operands inst
      sp = get_stack_reg modes
      arch_bit_size = get_arch_bit_size modes
      op_size = convert $ size dst
      -- Is the ESP register is used as a base register for addressing a destination operand in memory?
      sp_base = case (value dst) of
        (Mem mem_struct) -> isSubregisterOf (fromX86Reg (base mem_struct)) sp
        _ -> False
      -- Is the destination register is SP?
      sp_reg = case (value dst) of
        (Reg reg) -> isSubregisterOf (fromX86Reg reg) sp
        _ -> False
      -- An expression of the amount the stack pointer will be increased by
      delta_val = BvExpr (bitVector (convert op_size) arch_bit_size)
  in
    [inc_insn_ptr modes inst]
    ++ (includeIf sp_base [SetReg sp (BvaddExpr (GetReg sp) delta_val)])
    ++ [store_stmt modes dst (Load op_size (if sp_base then (BvsubExpr (GetReg sp) delta_val) else (GetReg sp)))]
    ++ (includeIf (not (sp_base || sp_reg)) [SetReg sp (BvaddExpr (GetReg sp) delta_val)])

-- Make list of operations in the IR that has the same semantics as the X86 mov instruction

mov_s :: [CsMode] -> CsInsn -> [Stmt]

mov_s modes inst =
  let (dst_op : src_op : _ ) = x86operands inst
      dst_ast = getOperandAst modes dst_op
      src_ast = getOperandAst modes src_op
      dst_size_bit = (convert $ size dst_op) * 8
      -- Segment registers are defined as 32 or 64 bit vectors in order to
      -- avoid having to simulate the GDT. This definition allows users to
      -- directly define their segments offset.
      node = (case (value dst_op) of
        (Reg reg) | is_segment_reg reg -> ExtractExpr 0 word_size_bit tmp_node
        _ -> tmp_node)
        where tmp_node = case (value src_op) of
                (Reg reg) | is_segment_reg reg -> ExtractExpr 0 dst_size_bit src_ast
                _ -> src_ast
      undef = case (value src_op) of
        (Reg reg) | is_control_reg reg -> True
        _ -> case (value dst_op) of
          (Reg reg) | is_control_reg reg -> True
          _ -> False
  in
    [inc_insn_ptr modes inst,
    store_stmt modes dst_op node]
    ++ includeIf undef
        [SetReg (fromX86Flag X86FlagAf) UndefinedExpr,
        SetReg (fromX86Flag X86FlagPf) UndefinedExpr,
        SetReg (fromX86Flag X86FlagSf) UndefinedExpr,
        SetReg (fromX86Flag X86FlagZf) UndefinedExpr,
        SetReg (fromX86Flag X86FlagCf) UndefinedExpr,
        SetReg (fromX86Flag X86FlagOf) UndefinedExpr]


-- Make a list of operations in the IR that has the same semantics as the X86 jmp instruction

jmp_s :: [CsMode] -> CsInsn -> [Stmt]

jmp_s modes inst =
  let (src_op : _ ) = x86operands inst
      src_ast = getOperandAst modes src_op
  in [SetReg (get_insn_ptr modes) src_ast]

-- Make a list of operations in the IR that has the same semantics as the X86 je instruction

je_s :: [CsMode] -> CsInsn -> [Stmt]

je_s modes inst =
  let (src_op : _ ) = x86operands inst
      src_ast = getOperandAst modes src_op
  in [SetReg (get_insn_ptr modes)
      (IteExpr (EqualExpr (GetReg (fromX86Flag X86FlagZf)) (BvExpr (bitVector 1 1)))
        src_ast
        (next_instr_ptr modes inst))]

-- Make list of operations in the IR that has the same semantics as the X86 lea instruction

lea_s :: [CsMode] -> CsInsn -> [Stmt]

lea_s modes inst =
  let (dst_op : src_op : _ ) = x86operands inst
      dst_ast = getOperandAst modes dst_op
      dst_size = fromIntegral (size dst_op * 8)
      src_ea = getLeaAst modes (getMem (value src_op))
      src_ea_size = get_arch_bit_size modes
      src_ea_fitted =
        if dst_size > src_ea_size then
          ZxExpr (dst_size - src_ea_size) src_ea
        else if dst_size < src_ea_size then
          ExtractExpr 0 dst_size src_ea
        else src_ea
  in [
      inc_insn_ptr modes inst,
      store_stmt modes dst_op src_ea_fitted
    ]

getCsX86arch :: Maybe CsDetail -> Maybe CsX86
getCsX86arch inst =
  let arch = maybe Nothing archInfo inst
  in case arch of
    Just (X86 csx86) -> Just csx86
    _ -> Nothing

x86operands :: CsInsn -> [CsX86Op]
x86operands inst =
  let arch2 = getCsX86arch (Capstone.detail inst)
      ops = maybe [] operands arch2
  in ops
