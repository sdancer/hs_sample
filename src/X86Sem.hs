module X86Sem where

import Ast
import AstContext (getOperandAst)
import Hapstone.Capstone
import Hapstone.Internal.Capstone as Capstone
import Hapstone.Internal.X86      as X86
import Util
import AstContext
import Data.Maybe
import Data.Word

--ll, ml, hl

-- Make operation to set the zero flag to the value that it would have after some operation

zf_s :: Expr -> CsX86Op -> Stmt
zf_s parent dst =
  let bv_size = (convert $ size dst) * 8 in
    set_flag X86FlagZf (IteExpr
      (EqualExpr (ExtractExpr (bv_size - 1) 0 parent) (BvExpr 0 bv_size))
      (BvExpr 1 1)
      (BvExpr 0 1))

-- Make operation to set the overflow flag to the value that it would have after an add operation

of_add_s :: Expr -> CsX86Op -> Expr -> Expr -> Stmt
of_add_s parent dst op1ast op2ast =
  let bv_size = (convert $ size dst) * 8 in
    set_flag X86FlagOf (ExtractExpr (bv_size - 1) (bv_size - 1)
      (BvandExpr
        (BvxorExpr op1ast (BvnotExpr op2ast))
        (BvxorExpr op1ast (ExtractExpr (bv_size - 1) 0 parent))))

-- Make operation to set the carry flag to the value that it would have after an add operation

cf_add_s :: Expr -> CsX86Op -> Expr -> Expr -> Stmt
cf_add_s parent dst op1ast op2ast =
  let bv_size = (convert $ size dst) * 8 in
    set_flag X86FlagCf (ExtractExpr (bv_size - 1) (bv_size - 1)
      (BvxorExpr (BvandExpr op1ast op2ast)
        (BvandExpr (BvxorExpr
            (BvxorExpr op1ast op2ast)
            (ExtractExpr (bv_size - 1) 0 parent))
          (BvxorExpr op1ast op2ast))))

-- Make operation to set the adjust flag to the value that it would have after some operation

af_s :: Expr -> CsX86Op -> Expr -> Expr -> Stmt
af_s parent dst op1ast op2ast =
  let bv_size = (convert $ size dst) * 8 in
    set_flag X86FlagAf (IteExpr
      (EqualExpr
        (BvExpr 0x10 bv_size)
        (BvandExpr
          (BvExpr 0x10 bv_size)
          (BvxorExpr
            (ExtractExpr (bv_size - 1) 0 parent)
            (BvxorExpr op1ast op2ast))))
      (BvExpr 1 1)
      (BvExpr 0 1))

-- Make operation to set the parity flag to the value that it would have after some operation

pf_s :: Expr -> CsX86Op -> Stmt
pf_s parent dst =
  let loop counter =
        (if counter == byte_size_bit
          then (BvExpr 1 1)
          else (BvxorExpr
            (loop (counter + 1))
            (ExtractExpr 0 0
              (BvlshrExpr
                (ExtractExpr 7 0 parent)
                (BvExpr counter byte_size_bit))))) in
    set_flag X86FlagPf (loop 0)

-- Make operation to set the sign flag to the value that it would have after some operation

sf_s :: Expr -> CsX86Op -> Stmt
sf_s parent dst =
  let bv_size = (convert $ size dst) * 8 in
    set_flag X86FlagSf (ExtractExpr (bv_size - 1) (bv_size - 1) parent)

-- Make list of operations in the IR that has the same semantics as the X86 add instruction

add_s :: CsInsn -> [Stmt]
add_s inst =
  let (op1 : op2 : _ ) = x86operands inst
      op1ast = getOperandAst op1
      op2ast = getOperandAst op2
      add_node = (BvaddExpr op1ast op2ast)
  in [
      store_node op1 add_node,
      af_s add_node op1 op1ast op1ast,
      pf_s add_node op1,
      sf_s add_node op1,
      zf_s add_node op1,
      cf_add_s add_node op1 op1ast op1ast,
      of_add_s add_node op1 op1ast op1ast
    ]

-- Make operation to set the carry flag to the value that it would have after an sub operation

cf_sub_s :: Expr -> CsX86Op -> Expr -> Expr -> Stmt
cf_sub_s parent dst op1ast op2ast =
  let bv_size = (convert $ size dst) * 8 in
    set_flag X86FlagCf (ExtractExpr (bv_size - 1) (bv_size - 1)
      (BvxorExpr
        (BvxorExpr op1ast (BvxorExpr op2ast (ExtractExpr (bv_size - 1) 0 parent)))
        (BvandExpr
          (BvxorExpr op1ast (ExtractExpr (bv_size - 1) 0 parent))
          (BvxorExpr op1ast op2ast))))

-- Make operation to set the overflow flag to the value that it would have after an sub operation

of_sub_s :: Expr -> CsX86Op -> Expr -> Expr -> Stmt
of_sub_s parent dst op1ast op2ast =
  let bv_size = (convert $ size dst) * 8 in
    set_flag X86FlagOf (ExtractExpr (bv_size - 1) (bv_size - 1)
      (BvandExpr
        (BvxorExpr op1ast op2ast)
        (BvxorExpr op1ast (ExtractExpr (bv_size - 1) 0 parent))))

-- Make list of operations in the IR that has the same semantics as the X86 sub instruction

sub_s :: CsInsn -> [Stmt]
sub_s inst =
  let (op1 : op2 : _ ) = x86operands inst
      op1ast = getOperandAst op1
      op2ast = getOperandAst op2
      sub_node = (BvsubExpr op1ast op2ast)
  in [
      store_node op1 sub_node,
      af_s sub_node op1 op1ast op1ast,
      pf_s sub_node op1,
      sf_s sub_node op1,
      zf_s sub_node op1,
      cf_sub_s sub_node op1 op1ast op1ast,
      of_sub_s sub_node op1 op1ast op1ast
    ]

-- Make list of operations in the IR that has the same semantics as the X86 xor instruction

xor_s :: CsInsn -> [Stmt]
xor_s inst =
  let (dst_op : src_op : _ ) = x86operands inst
      dst_ast = getOperandAst dst_op
      src_ast = getOperandAst src_op
      xor_node = (BvxorExpr dst_ast src_ast)
  in [
      store_node dst_op xor_node,
      set_flag X86FlagAf UndefinedExpr,
      pf_s xor_node dst_op,
      sf_s xor_node dst_op,
      zf_s xor_node dst_op,
      set_flag X86FlagCf (BvExpr 0 1),
      set_flag X86FlagOf (BvExpr 0 1)
    ]

-- Make list of operations in the IR that has the same semantics as the X86 and instruction

and_s :: CsInsn -> [Stmt]
and_s inst =
  let (dst_op : src_op : _ ) = x86operands inst
      dst_ast = getOperandAst dst_op
      src_ast = getOperandAst src_op
      and_node = (BvandExpr dst_ast src_ast)
  in [
      store_node dst_op and_node,
      set_flag X86FlagAf UndefinedExpr,
      pf_s and_node dst_op,
      sf_s and_node dst_op,
      zf_s and_node dst_op,
      set_flag X86FlagCf (BvExpr 0 1),
      set_flag X86FlagOf (BvExpr 0 1)
    ]

-- Make list of operations in the IR that has the same semantics as the X86 or instruction

or_s :: CsInsn -> [Stmt]
or_s inst =
  let (dst_op : src_op : _ ) = x86operands inst
      dst_ast = getOperandAst dst_op
      src_ast = getOperandAst src_op
      and_node = (BvorExpr dst_ast src_ast)
  in [
      store_node dst_op and_node,
      set_flag X86FlagAf UndefinedExpr,
      pf_s and_node dst_op,
      sf_s and_node dst_op,
      zf_s and_node dst_op,
      set_flag X86FlagCf (BvExpr 0 1),
      set_flag X86FlagOf (BvExpr 0 1)
    ]

-- Make list of operations in the IR that has the same semantics as the X86 push instruction

push_s :: [CsMode] -> CsInsn -> [Stmt]
push_s modes inst =
  let (op1 : _) = x86operands inst
      sp = (get_stack_reg modes)
      arch_size = get_arch_size modes
      -- If it's an immediate source, the memory access is always based on the arch size
      op_size = case (value op1) of
        (Imm _) -> arch_size
        _ -> convert $ size op1
  in [
      SetReg sp (BvsubExpr (GetReg sp) (BvExpr op_size (arch_size * 8))),
      Store op_size (GetReg sp) (ZxExpr ((op_size - (convert $ size op1)) * 8) (getOperandAst op1))
    ]

-- Makes a singleton list containing the argument if the condition is true. Otherwise makes
-- the empty list.

includeIf :: Bool -> [a] -> [a]
includeIf cond sublist = if cond then sublist else []

-- Make list of operations in the IR that has the same semantics as the X86 pop instruction

pop_s :: [CsMode] -> CsInsn -> [Stmt]
pop_s modes inst =
  let (op1 : _) = x86operands inst
      sp = get_stack_reg modes
      arch_size = get_arch_size modes
      op_size = convert $ size op1
      -- Is the ESP register is used as a base register for addressing a destination operand in memory?
      sp_base = case (value op1) of
        (Mem mem_struct) -> isSubregisterOf (compoundReg (base mem_struct)) sp
        _ -> False
      -- Is the destination register is SP?
      sp_reg = case (value op1) of
        (Reg reg) -> isSubregisterOf (compoundReg reg) sp
        _ -> False
      -- An expression of the amount the stack pointer will be increased by
      delta_val = (BvExpr op_size (arch_size * 8))
  in
    (includeIf sp_base [SetReg sp (BvaddExpr (GetReg sp) delta_val)])
    ++ [store_node op1 (Read op_size (if sp_base then (BvsubExpr (GetReg sp) delta_val) else (GetReg sp)))]
    ++ (includeIf (not (sp_base || sp_reg)) [SetReg sp (BvaddExpr (GetReg sp) delta_val)])

-- Make list of operations in the IR that has the same semantics as the X86 mov instruction

mov_s ::  CsInsn -> [Stmt]
mov_s inst =
  let (dst_op : src_op : _ ) = x86operands inst
      dst_ast = getOperandAst dst_op
      src_ast = getOperandAst src_op
      dst_size_bit = (convert $ size dst_op) * 8
      -- Segment registers are defined as 32 or 64 bit vectors in order to
      -- avoid having to simulate the GDT. This definition allows users to
      -- directly define their segments offset.
      node = (case (value dst_op) of
        (Reg reg) | is_segment_reg reg -> ExtractExpr (word_size_bit - 1) 0 tmp_node
        _ -> tmp_node)
        where tmp_node = case (value src_op) of
                (Reg reg) | is_segment_reg reg -> ExtractExpr (dst_size_bit - 1) 0 src_ast
                _ -> src_ast
      undef = case (value src_op) of
        (Reg reg) | is_control_reg reg -> True
        _ -> case (value dst_op) of
          (Reg reg) | is_control_reg reg -> True
          _ -> False
  in
    [store_node dst_op node]
    ++ includeIf undef
        [set_flag X86FlagAf UndefinedExpr,
        set_flag X86FlagPf UndefinedExpr,
        set_flag X86FlagSf UndefinedExpr,
        set_flag X86FlagZf UndefinedExpr,
        set_flag X86FlagCf UndefinedExpr,
        set_flag X86FlagOf UndefinedExpr]

-- Make a list of operations in the IR that has the same semantics as the X86 jmp instruction

jmp_s :: CsInsn -> [Stmt]
jmp_s inst =
  let (src_op : _ ) = x86operands inst
      src_ast = getOperandAst src_op
  in [Branch (BvExpr 1 1) src_ast]

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

get_stack_reg :: [CsMode] -> CompoundReg
get_stack_reg modes =
  if elem CsMode32 modes then compoundReg X86RegEsp
  else if elem CsMode32 modes then compoundReg X86RegRsp
  else error "Processor modes underspecified."

get_arch_size :: [CsMode] -> Int
get_arch_size modes =
  if elem CsMode32 modes then 4
  else if elem CsMode32 modes then 8
  else error "Processor modes underspecified."

store_node :: CsX86Op -> Expr -> Stmt
store_node operand store_what =
  case (value operand) of
    (Reg reg) -> SetReg (compoundReg reg) store_what
    (Mem mem) -> Store (convert $ size operand) (getLeaAst mem) store_what
    _ -> error "Target of store operation is neither a register nor a memory operand."

set_flag flag expr =
  let compReg = compoundReg X86RegEflags
      (low, high) = flagToBit flag
    in SetReg compReg (ReplaceExpr high low (GetReg compReg) expr)
    
