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

next_instr_ptr modes insn = BvExpr (toBv (insnAddr + insnLen) archBitSize)
  where insnAddr = fromIntegral (address insn)
        insnLen = fromIntegral (length (bytes insn))
        archBitSize = fromIntegral (get_arch_bit_size modes)

-- Make operation to increase instruction pointer by size of instruction

inc_insn_ptr :: [CsMode] -> CsInsn -> Stmt a

inc_insn_ptr modes insn = SetReg undefined (get_insn_ptr modes) (next_instr_ptr modes insn)

-- Gets an expression for the given operand. Processor mode may be needed for computing
-- effective addresses.

getOperandAst :: [CsMode] -> CsX86Op -> Expr

getOperandAst modes op =
  let opBitSize = fromIntegral (size op) * byte_size_bit
  in case value op of
    (Imm value) -> BvExpr (toBv value opBitSize)
    (Reg reg) -> GetReg (fromX86Reg reg)
    (Mem mem) -> Load opBitSize (getLeaAst modes mem)

-- Gets the specified register after it has been zero extended to the architecture size

getZxRegister :: [CsMode] -> CompoundReg -> Expr

getZxRegister modes reg = ZxExpr (get_arch_bit_size modes) (GetReg reg)

-- Gets an expression for the memory location of the given memory operand

getLeaAst :: [CsMode] -> X86OpMemStruct -> Expr

getLeaAst modes mem =
  (BvaddExpr node_disp (BvaddExpr node_base node_index)) where
    arch_bit_size = get_arch_bit_size modes
    node_base = case base mem of
      X86RegInvalid -> BvExpr (toBv 0 arch_bit_size)
      reg -> getZxRegister modes (fromX86Reg reg)
    node_index = case index mem of
      X86RegInvalid -> BvExpr (toBv 0 arch_bit_size)
      reg -> BvmulExpr (getZxRegister modes (fromX86Reg reg)) (BvExpr (toBv (scale mem) arch_bit_size))
    node_disp = BvExpr (toBv (disp' mem) arch_bit_size)

-- Make operation to store the given expression in the given operand

store_stmt :: [CsMode] -> CsX86Op -> Expr -> Stmt a

store_stmt modes operand store_what =
  case (value operand) of
    (Reg reg) -> SetReg undefined (fromX86Reg reg) store_what
    (Mem mem) -> Store undefined (getLeaAst modes mem) store_what
    _ -> Comment ("Target of store operation is neither a register nor a memory operand.")

-- Make operation to set the zero flag to the value that it would have after some operation

zf_s :: Expr -> CsX86Op -> Stmt a

zf_s parent dst =
  let bv_size = (fromIntegral $ size dst) * 8 in
    SetReg undefined (fromX86Flag X86FlagZf) (IteExpr
      (EqualExpr (ExtractExpr 0 bv_size parent) (BvExpr (toBv 0 bv_size)))
      (BvExpr (toBv 1 1))
      (BvExpr (toBv 0 1)))

-- Make operation to set the overflow flag to the value that it would have after an add operation

of_add_s :: Expr -> CsX86Op -> Expr -> Expr -> Stmt a

of_add_s parent dst op1ast op2ast =
  let bv_size = (fromIntegral $ size dst) * 8 in
    SetReg undefined (fromX86Flag X86FlagOf) (ExtractExpr (bv_size - 1) bv_size
      (BvandExpr
        (BvxorExpr op1ast (BvnotExpr op2ast))
        (BvxorExpr op1ast (ExtractExpr 0 bv_size parent))))

-- Make operation to set the carry flag to the value that it would have after an add operation

cf_add_s :: Expr -> CsX86Op -> Expr -> Expr -> Stmt a

cf_add_s parent dst op1ast op2ast =
  let bv_size = (fromIntegral $ size dst) * 8 in
    SetReg undefined (fromX86Flag X86FlagCf) (ExtractExpr (bv_size - 1) bv_size
      (BvxorExpr (BvandExpr op1ast op2ast)
        (BvandExpr (BvxorExpr
            (BvxorExpr op1ast op2ast)
            (ExtractExpr 0 bv_size parent))
          (BvxorExpr op1ast op2ast))))

-- Make operation to set the adjust flag to the value that it would have after some operation

af_s :: Expr -> CsX86Op -> Expr -> Expr -> Stmt a

af_s parent dst op1ast op2ast =
  let bv_size = (fromIntegral $ size dst) * 8 in
    SetReg undefined (fromX86Flag X86FlagAf) (IteExpr
      (EqualExpr
        (BvExpr (toBv 0x10 bv_size))
        (BvandExpr
          (BvExpr (toBv 0x10 bv_size))
          (BvxorExpr
            (ExtractExpr 0 bv_size parent)
            (BvxorExpr op1ast op2ast))))
      (BvExpr (toBv 1 1))
      (BvExpr (toBv 0 1)))

-- Make operation to set the parity flag to the value that it would have after some operation

pf_s :: Expr -> CsX86Op -> Stmt a

pf_s parent dst =
  let loop counter =
        (if counter == byte_size_bit
          then BvExpr (toBv 1 1)
          else (BvxorExpr
            (loop (counter + 1))
            (ExtractExpr 0 1
              (BvlshrExpr
                (ExtractExpr 0 byte_size_bit parent)
                (BvExpr (toBv counter byte_size_bit)))))) in
    SetReg undefined (fromX86Flag X86FlagPf) (loop 0)

-- Make operation to set the sign flag to the value that it would have after some operation

sf_s :: Expr -> CsX86Op -> Stmt a

sf_s parent dst =
  let bv_size = (fromIntegral $ size dst) * 8 in
    SetReg undefined (fromX86Flag X86FlagSf) (ExtractExpr (bv_size - 1) bv_size parent)

-- Make list of operations in the IR that has the same semantics as the X86 add instruction

add_s :: [CsMode] -> CsInsn -> [Stmt a]

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

inc_s :: [CsMode] -> CsInsn -> [Stmt a]

inc_s modes inst =
  let (dst : _ ) = x86operands inst
      dst_ast = getOperandAst modes dst
      src_ast = BvExpr (toBv 1 (fromIntegral (size dst * byte_size_bit)))
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

cf_sub_s :: Expr -> CsX86Op -> Expr -> Expr -> Stmt a

cf_sub_s parent dst op1ast op2ast =
  let bv_size = (fromIntegral $ size dst) * 8 in
    SetReg undefined (fromX86Flag X86FlagCf) (ExtractExpr (bv_size - 1) bv_size
      (BvxorExpr
        (BvxorExpr op1ast (BvxorExpr op2ast (ExtractExpr 0 bv_size parent)))
        (BvandExpr
          (BvxorExpr op1ast (ExtractExpr 0 bv_size parent))
          (BvxorExpr op1ast op2ast))))

-- Make operation to set the overflow flag to the value that it would have after an sub operation

of_sub_s :: Expr -> CsX86Op -> Expr -> Expr -> Stmt a

of_sub_s parent dst op1ast op2ast =
  let bv_size = (fromIntegral $ size dst) * 8 in
    SetReg undefined (fromX86Flag X86FlagOf) (ExtractExpr (bv_size - 1) bv_size
      (BvandExpr
        (BvxorExpr op1ast op2ast)
        (BvxorExpr op1ast (ExtractExpr 0 bv_size parent))))

-- Make list of operations in the IR that has the same semantics as the X86 sub instruction

sub_s :: [CsMode] -> CsInsn -> [Stmt a]

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

cmp_s :: [CsMode] -> CsInsn -> [Stmt a]

cmp_s modes inst =
  let (dst : src : _ ) = x86operands inst
      dst_ast = getOperandAst modes dst
      src_ast = SxExpr (fromIntegral (size dst) * byte_size_bit) (getOperandAst modes src)
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

xor_s :: [CsMode] -> CsInsn -> [Stmt a]

xor_s modes inst =
  let (dst_op : src_op : _ ) = x86operands inst
      dst_ast = getOperandAst modes dst_op
      src_ast = getOperandAst modes src_op
      xor_node = (BvxorExpr dst_ast src_ast)
  in [
      inc_insn_ptr modes inst,
      store_stmt modes dst_op xor_node,
      SetReg undefined (fromX86Flag X86FlagAf) (UndefinedExpr 1),
      pf_s xor_node dst_op,
      sf_s xor_node dst_op,
      zf_s xor_node dst_op,
      SetReg undefined (fromX86Flag X86FlagCf) (BvExpr (toBv 0 1)),
      SetReg undefined (fromX86Flag X86FlagOf) (BvExpr (toBv 0 1))
    ]

-- Make list of operations in the IR that has the same semantics as the X86 and instruction

and_s :: [CsMode] -> CsInsn -> [Stmt a]

and_s modes inst =
  let (dst_op : src_op : _ ) = x86operands inst
      dst_ast = getOperandAst modes dst_op
      src_ast = getOperandAst modes src_op
      and_node = (BvandExpr dst_ast src_ast)
  in [
      inc_insn_ptr modes inst,
      store_stmt modes dst_op and_node,
      SetReg undefined (fromX86Flag X86FlagAf) (UndefinedExpr 1),
      pf_s and_node dst_op,
      sf_s and_node dst_op,
      zf_s and_node dst_op,
      SetReg undefined (fromX86Flag X86FlagCf) (BvExpr (toBv 0 1)),
      SetReg undefined (fromX86Flag X86FlagOf) (BvExpr (toBv 0 1))
    ]

-- Make list of operations in the IR that has the same semantics as the X86 test instruction

test_s :: [CsMode] -> CsInsn -> [Stmt a]

test_s modes inst =
  let (dst_op : src_op : _ ) = x86operands inst
      dst_ast = getOperandAst modes dst_op
      src_ast = getOperandAst modes src_op
      test_node = (BvandExpr dst_ast src_ast)
  in [
      inc_insn_ptr modes inst,
      SetReg undefined (fromX86Flag X86FlagAf) (UndefinedExpr 1),
      pf_s test_node dst_op,
      sf_s test_node dst_op,
      zf_s test_node dst_op,
      SetReg undefined (fromX86Flag X86FlagCf) (BvExpr (toBv 0 1)),
      SetReg undefined (fromX86Flag X86FlagOf) (BvExpr (toBv 0 1))
    ]

-- Make list of operations in the IR that has the same semantics as the X86 or instruction

or_s :: [CsMode] -> CsInsn -> [Stmt a]

or_s modes inst =
  let (dst_op : src_op : _ ) = x86operands inst
      dst_ast = getOperandAst modes dst_op
      src_ast = getOperandAst modes src_op
      and_node = (BvorExpr dst_ast src_ast)
  in [
      inc_insn_ptr modes inst,
      store_stmt modes dst_op and_node,
      SetReg undefined (fromX86Flag X86FlagAf) (UndefinedExpr 1),
      pf_s and_node dst_op,
      sf_s and_node dst_op,
      zf_s and_node dst_op,
      SetReg undefined (fromX86Flag X86FlagCf) (BvExpr (toBv 0 1)),
      SetReg undefined (fromX86Flag X86FlagOf) (BvExpr (toBv 0 1))
    ]

-- Make list of operations in the IR that has the same semantics as the X86 push instruction

push_s :: [CsMode] -> CsInsn -> [Stmt a]

push_s modes inst =
  let (src : _) = x86operands inst
      sp = get_stack_reg modes
      arch_byte_size = get_arch_byte_size modes
      -- If it's an immediate source, the memory access is always based on the arch size
      op_size = case (value src) of
        (Imm _) -> arch_byte_size
        _ -> fromIntegral $ size src
  in [
      inc_insn_ptr modes inst,
      SetReg undefined sp (BvsubExpr (GetReg sp) (BvExpr (toBv op_size (arch_byte_size * byte_size_bit)))),
      Store undefined (GetReg sp) (ZxExpr (op_size * byte_size_bit) (getOperandAst modes src))
    ]

-- Makes a singleton list containing the argument if the condition is true. Otherwise makes
-- the empty list.

includeIf :: Bool -> [a] -> [a]

includeIf cond sublist = if cond then sublist else []

-- Make list of operations in the IR that has the same semantics as the X86 pop instruction

pop_s :: [CsMode] -> CsInsn -> [Stmt a]

pop_s modes inst =
  let (dst : _) = x86operands inst
      sp = get_stack_reg modes
      arch_bit_size = get_arch_bit_size modes
      op_size = fromIntegral $ size dst
      -- Is the ESP register is used as a base register for addressing a destination operand in memory?
      sp_base = case (value dst) of
        (Mem mem_struct) -> isSubregisterOf (fromX86Reg (base mem_struct)) sp
        _ -> False
      -- Is the destination register is SP?
      sp_reg = case (value dst) of
        (Reg reg) -> isSubregisterOf (fromX86Reg reg) sp
        _ -> False
      -- An expression of the amount the stack pointer will be increased by
      delta_val = BvExpr (toBv op_size arch_bit_size)
  in
    [inc_insn_ptr modes inst]
    ++ (includeIf sp_base [SetReg undefined sp (BvaddExpr (GetReg sp) delta_val)])
    ++ [store_stmt modes dst
        (Load (op_size * byte_size_bit)
          (if sp_base then (BvsubExpr (GetReg sp) delta_val) else (GetReg sp)))]
    ++ (includeIf (not (sp_base || sp_reg)) [SetReg undefined sp (BvaddExpr (GetReg sp) delta_val)])

-- Make list of operations in the IR that has the same semantics as the X86 mov instruction

mov_s :: [CsMode] -> CsInsn -> [Stmt a]

mov_s modes inst =
  let (dst_op : src_op : _ ) = x86operands inst
      dst_ast = getOperandAst modes dst_op
      src_ast = getOperandAst modes src_op
      dst_size_bit = (fromIntegral $ size dst_op) * byte_size_bit
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
        [SetReg undefined (fromX86Flag X86FlagAf) (UndefinedExpr 1),
        SetReg undefined (fromX86Flag X86FlagPf) (UndefinedExpr 1),
        SetReg undefined (fromX86Flag X86FlagSf) (UndefinedExpr 1),
        SetReg undefined (fromX86Flag X86FlagZf) (UndefinedExpr 1),
        SetReg undefined (fromX86Flag X86FlagCf) (UndefinedExpr 1),
        SetReg undefined (fromX86Flag X86FlagOf) (UndefinedExpr 1)]

-- Make list of operations in the IR that has the same semantics as the X86 movzx instruction

movzx_s :: [CsMode] -> CsInsn -> [Stmt a]

movzx_s modes inst =
  let (dst_op : src_op : _ ) = x86operands inst
      dst_ast = getOperandAst modes dst_op
      src_ast = getOperandAst modes src_op
      dst_size_bit = (fromIntegral $ size dst_op) * byte_size_bit
      zx_node = ZxExpr dst_size_bit src_ast
  in
    [inc_insn_ptr modes inst,
    store_stmt modes dst_op zx_node]

-- Make a list of operations in the IR that has the same semantics as the X86 jmp instruction

jmp_s :: [CsMode] -> CsInsn -> [Stmt a]

jmp_s modes inst =
  let (src_op : _ ) = x86operands inst
      src_ast = getOperandAst modes src_op
  in [SetReg undefined (get_insn_ptr modes) src_ast]

-- Make a list of operations in the IR that has the same semantics as the X86 je instruction

je_s :: [CsMode] -> CsInsn -> [Stmt a]

je_s modes inst =
  let (src_op : _ ) = x86operands inst
      src_ast = getOperandAst modes src_op
      insn_ptr = get_insn_ptr modes
  in
      [SetReg undefined insn_ptr
        (IteExpr (EqualExpr (GetReg (fromX86Flag X86FlagZf)) (BvExpr (toBv 1 1)))
          src_ast
          (next_instr_ptr modes inst))]

-- Make a list of operations in the IR that has the same semantics as the X86 jne instruction

jne_s :: [CsMode] -> CsInsn -> [Stmt a]

jne_s modes inst =
  let (src_op : _ ) = x86operands inst
      src_ast = getOperandAst modes src_op
      insn_ptr = get_insn_ptr modes
  in
      [SetReg undefined insn_ptr
        (IteExpr (EqualExpr (GetReg (fromX86Flag X86FlagZf)) (BvExpr (toBv 0 1)))
          src_ast
          (next_instr_ptr modes inst))]

-- Make a list of operations in the IR that has the same semantics as the X86 call instruction

call_s :: [CsMode] -> CsInsn -> [Stmt a]

call_s modes inst =
  let (src_op : _ ) = x86operands inst
      src_ast = getOperandAst modes src_op
      sp = get_stack_reg modes
      arch_byte_size = get_arch_byte_size modes
      arch_bit_size = get_arch_bit_size modes
  in
      [SetReg undefined sp (BvsubExpr (GetReg sp) (BvExpr (toBv arch_byte_size arch_bit_size))),
      Store undefined (GetReg sp) (next_instr_ptr modes inst),
      SetReg undefined (get_insn_ptr modes) (ZxExpr arch_bit_size src_ast)]

-- Make a list of operations in the IR that has the same semantics as the X86 ret instruction

ret_s :: [CsMode] -> CsInsn -> [Stmt a]

ret_s modes inst =
  let operands = x86operands inst
      sp = get_stack_reg modes
      insn_ptr = get_insn_ptr modes
      arch_byte_size = get_arch_byte_size modes
  in
      [SetReg undefined insn_ptr (GetReg sp),
      SetReg undefined sp (BvaddExpr (GetReg sp) (BvExpr (toBv arch_byte_size (arch_byte_size * byte_size_bit))))]
      ++ includeIf (length operands > 0)
          (let src_ast = getOperandAst modes (head operands)
          in [SetReg undefined sp (BvaddExpr (GetReg sp) (ZxExpr (arch_byte_size * byte_size_bit) src_ast))])

-- Make list of operations in the IR that has the same semantics as the X86 lea instruction

lea_s :: [CsMode] -> CsInsn -> [Stmt a]

lea_s modes inst =
  let (dst_op : src_op : _ ) = x86operands inst
      dst_ast = getOperandAst modes dst_op
      dst_size = fromIntegral (size dst_op * byte_size_bit)
      src_ea_size = get_arch_bit_size modes
  in [
      inc_insn_ptr modes inst,
      case value src_op of
        Imm value -> Comment ("Memory operand expected in " ++ show inst ++ ", but immediate value found instead. Ignoring opcode.")
        Reg reg -> Comment ("Memory operand expected in " ++ show inst ++ ", but register value found instead. Ignoring opcode.")
        Mem mem ->
          let src_ea = getLeaAst modes mem
              src_ea_fitted =
                if dst_size > src_ea_size then
                  ZxExpr dst_size src_ea
                else if dst_size < src_ea_size then
                  ExtractExpr 0 dst_size src_ea
                else src_ea
          in store_stmt modes dst_op src_ea_fitted
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

