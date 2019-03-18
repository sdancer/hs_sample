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

zf_s :: AstNode -> CsX86Op -> Stmt
zf_s parent dst =
  let bv_size = (size dst) * 8 in
    set_flag X86FlagZf (IteNode
      (EqualNode (ExtractNode (bv_size - 1) 0 parent) (BvNode 0 bv_size))
      (BvNode 1 1)
      (BvNode 0 1))

-- Make operation to set the overflow flag to the value that it would have after an add operation

of_add_s :: AstNode -> CsX86Op -> AstNode -> AstNode -> Stmt
of_add_s parent dst op1ast op2ast =
  let bv_size = (size dst) * 8 in
    set_flag X86FlagOf (ExtractNode (bv_size - 1) (bv_size - 1)
      (BvandNode
        (BvxorNode op1ast (BvnotNode op2ast))
        (BvxorNode op1ast (ExtractNode (bv_size - 1) 0 parent))))

-- Make operation to set the carry flag to the value that it would have after an add operation

cf_add_s :: AstNode -> CsX86Op -> AstNode -> AstNode -> Stmt
cf_add_s parent dst op1ast op2ast =
  let bv_size = (size dst) * 8 in
    set_flag X86FlagCf (ExtractNode (bv_size - 1) (bv_size - 1)
      (BvxorNode (BvandNode op1ast op2ast)
        (BvandNode (BvxorNode
            (BvxorNode op1ast op2ast)
            (ExtractNode (bv_size - 1) 0 parent))
          (BvxorNode op1ast op2ast))))

-- Make operation to set the adjust flag to the value that it would have after some operation

af_s :: AstNode -> CsX86Op -> AstNode -> AstNode -> Stmt
af_s parent dst op1ast op2ast =
  let bv_size = (size dst) * 8 in
    set_flag X86FlagAf (IteNode
      (EqualNode
        (BvNode 0x10 bv_size)
        (BvandNode
          (BvNode 0x10 bv_size)
          (BvxorNode
            (ExtractNode (bv_size - 1) 0 parent)
            (BvxorNode op1ast op2ast))))
      (BvNode 1 1)
      (BvNode 0 1))

-- Make operation to set the parity flag to the value that it would have after some operation

pf_s :: AstNode -> CsX86Op -> Stmt
pf_s parent dst =
  let loop counter =
        (if counter == byte_size_bit
          then (BvNode 1 1)
          else (BvxorNode
            (loop (counter + 1))
            (ExtractNode 0 0
              (BvlshrNode
                (ExtractNode 7 0 parent)
                (BvNode (fromIntegral counter) byte_size_bit))))) in
    set_flag X86FlagPf (loop 0)

-- Make operation to set the sign flag to the value that it would have after some operation

sf_s :: AstNode -> CsX86Op -> Stmt
sf_s parent dst =
  let bv_size = (size dst) * 8 in
    set_flag X86FlagSf (ExtractNode (bv_size - 1) (bv_size - 1) parent)

-- Make list of operations in the IR that has the same semantics as the X86 add instruction

add_s :: CsInsn -> [Stmt]
add_s inst =
  let (op1 : op2 : _ ) = x86operands inst
      op1ast = getOperandAst op1
      op2ast = getOperandAst op2
      add_node = (BvaddNode op1ast op2ast)
  in [
      store_node (value op1) add_node,
      af_s add_node op1 op1ast op1ast,
      pf_s add_node op1,
      sf_s add_node op1,
      zf_s add_node op1,
      cf_add_s add_node op1 op1ast op1ast,
      of_add_s add_node op1 op1ast op1ast
    ]

-- Make operation to set the carry flag to the value that it would have after an sub operation

cf_sub_s :: AstNode -> CsX86Op -> AstNode -> AstNode -> Stmt
cf_sub_s parent dst op1ast op2ast =
  let bv_size = (size dst) * 8 in
    set_flag X86FlagCf (ExtractNode (bv_size - 1) (bv_size - 1)
      (BvxorNode
        (BvxorNode op1ast (BvxorNode op2ast (ExtractNode (bv_size - 1) 0 parent)))
        (BvandNode
          (BvxorNode op1ast (ExtractNode (bv_size - 1) 0 parent))
          (BvxorNode op1ast op2ast))))

-- Make operation to set the overflow flag to the value that it would have after an sub operation

of_sub_s :: AstNode -> CsX86Op -> AstNode -> AstNode -> Stmt
of_sub_s parent dst op1ast op2ast =
  let bv_size = (size dst) * 8 in
    set_flag X86FlagOf (ExtractNode (bv_size - 1) (bv_size - 1)
      (BvandNode
        (BvxorNode op1ast op2ast)
        (BvxorNode op1ast (ExtractNode (bv_size - 1) 0 parent))))

-- Make list of operations in the IR that has the same semantics as the X86 sub instruction

sub_s :: CsInsn -> [Stmt]
sub_s inst =
  let (op1 : op2 : _ ) = x86operands inst
      op1ast = getOperandAst op1
      op2ast = getOperandAst op2
      sub_node = (BvsubNode op1ast op2ast)
  in [
      store_node (value op1) sub_node,
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
      xor_node = (BvxorNode dst_ast src_ast)
  in [
      store_node (value dst_op) xor_node,
      set_flag X86FlagAf UndefinedNode,
      pf_s xor_node dst_op,
      sf_s xor_node dst_op,
      zf_s xor_node dst_op,
      set_flag X86FlagCf (BvNode 0 1),
      set_flag X86FlagOf (BvNode 0 1)
    ]

-- Make list of operations in the IR that has the same semantics as the X86 and instruction

and_s :: CsInsn -> [Stmt]
and_s inst =
  let (dst_op : src_op : _ ) = x86operands inst
      dst_ast = getOperandAst dst_op
      src_ast = getOperandAst src_op
      and_node = (BvandNode dst_ast src_ast)
  in [
      store_node (value dst_op) and_node,
      set_flag X86FlagAf UndefinedNode,
      pf_s and_node dst_op,
      sf_s and_node dst_op,
      zf_s and_node dst_op,
      set_flag X86FlagCf (BvNode 0 1),
      set_flag X86FlagOf (BvNode 0 1)
    ]

-- Make list of operations in the IR that has the same semantics as the X86 or instruction

or_s :: CsInsn -> [Stmt]
or_s inst =
  let (dst_op : src_op : _ ) = x86operands inst
      dst_ast = getOperandAst dst_op
      src_ast = getOperandAst src_op
      and_node = (BvorNode dst_ast src_ast)
  in [
      store_node (value dst_op) and_node,
      set_flag X86FlagAf UndefinedNode,
      pf_s and_node dst_op,
      sf_s and_node dst_op,
      zf_s and_node dst_op,
      set_flag X86FlagCf (BvNode 0 1),
      set_flag X86FlagOf (BvNode 0 1)
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
        _ -> size op1
  in [
      SetReg sp (BvsubNode (GetReg sp) (BvNode (convert op_size) (arch_size * 8))),
      Store (GetReg sp) (ZxNode (convert ((op_size - (size op1)) * 8)) (getOperandAst op1))
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
      op_size = convert (size op1)
      -- Is the ESP register is used as a base register for addressing a destination operand in memory?
      sp_base = case (value op1) of
        (Mem mem_struct) -> isSubregisterOf (compoundReg (base mem_struct)) sp
        _ -> False
      -- Is the destination register is SP?
      sp_reg = case (value op1) of
        (Reg reg) -> isSubregisterOf (compoundReg reg) sp
        _ -> False
      -- The new value of the stack pointer
      new_sp_val = BvaddNode (GetReg sp) (BvNode op_size (arch_size * 8))
  in
    (includeIf sp_base [SetReg sp new_sp_val])
    ++ [store_node (value op1) (Read new_sp_val)]
    ++ (includeIf (not (sp_base || sp_reg)) [SetReg sp new_sp_val])

-- Make list of operations in the IR that has the same semantics as the X86 mov instruction

mov ::  CsInsn -> [Stmt]
mov inst =
  let (dst_op : src_op : _ ) = x86operands inst
      dst_ast = getOperandAst dst_op
      src_ast = getOperandAst src_op
      dst_size_bit = (size dst_op) * 8
      -- Segment registers are defined as 32 or 64 bit vectors in order to
      -- avoid having to simulate the GDT. This definition allows users to
      -- directly define their segments offset.
      node = (case (value dst_op) of
        (Reg reg) | is_segment_reg reg -> ExtractNode (word_size_bit - 1) 0 tmp_node
        _ -> tmp_node)
        where tmp_node = case (value src_op) of
                (Reg reg) | is_segment_reg reg -> ExtractNode (dst_size_bit - 1) 0 src_ast
                _ -> src_ast
      undef = case (value src_op) of
        (Reg reg) | is_control_reg reg -> True
        _ -> case (value dst_op) of
          (Reg reg) | is_control_reg reg -> True
          _ -> False
  in
    [store_node (value dst_op) node]
    ++ includeIf undef
        [set_flag X86FlagAf UndefinedNode,
        set_flag X86FlagPf UndefinedNode,
        set_flag X86FlagSf UndefinedNode,
        set_flag X86FlagZf UndefinedNode,
        set_flag X86FlagCf UndefinedNode,
        set_flag X86FlagOf UndefinedNode]

getCsX86arch :: Maybe CsDetail -> Maybe CsX86
getCsX86arch inst =
            let arch = maybe Nothing archInfo inst
            in case arch of
              Just (X86 csx86) -> Just csx86
              _ -> Nothing

x86operands :: CsInsn -> [CsX86Op]
x86operands inst =
        let
              arch2 = getCsX86arch (Capstone.detail inst)
              ops = maybe [] operands arch2
        in
          ops

get_stack_reg :: [CsMode] -> CompoundReg
get_stack_reg modes =
  if elem CsMode32 modes then compoundReg X86RegEsp
  else if elem CsMode32 modes then compoundReg X86RegRsp
  else error "Processor modes underspecified."

get_arch_size :: [CsMode] -> Word8
get_arch_size modes =
  if elem CsMode32 modes then 4
  else if elem CsMode32 modes then 8
  else error "Processor modes underspecified."

--byte size is ignored
store_node :: CsX86OpValue -> AstNode -> Stmt
store_node operand store_what =
  case operand of
    (Reg reg) -> SetReg (compoundReg reg) store_what
    (Mem mem) -> Store (getLeaAst mem) store_what
    _ -> error "Target of store operation is neither a register nor a memory operand."

set_flag flag expr =
  let compReg = compoundReg X86RegEflags
      (low, high) = flagToBit flag
    in SetReg compReg (ReplaceNode (convert high) (convert low) (GetReg compReg) expr)
    
