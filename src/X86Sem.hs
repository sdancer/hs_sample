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

byte_size_bit = 8
word_size_bit = 16
dword_size_bit = 32
qword_size_bit = 64
dqword_size_bit = 128
qqword_size_bit = 256
dqqword_size_bit = 512

-- Given the target processor mode, get the largest register containing this register

get_parent_register :: [CsMode] -> X86Reg -> X86Reg

get_parent_register modes reg | elem CsMode16 modes =
  case reg of
    X86RegAl -> X86RegAx
    X86RegAh -> X86RegAx
    X86RegBl -> X86RegBx
    X86RegBh -> X86RegBx
    X86RegCl -> X86RegCx
    X86RegCh -> X86RegCx
    X86RegDl -> X86RegDx
    X86RegDh -> X86RegDx
    X86RegDil -> X86RegDi
    X86RegSil -> X86RegSi
    X86RegBpl -> X86RegBp
    X86RegSpl -> X86RegSp
    --X86RegR8l -> X86RegR8w
    --X86RegR9l -> X86RegR9w
    --X86RegR10l -> X86RegR10w
    --X86RegR11l -> X86RegR11w
    --X86RegR12l -> X86RegR12w
    --X86RegR13l -> X86RegR13w
    --X86RegR14l -> X86RegR14w
    --X86RegR15l -> X86RegR15w
    otherwise -> otherwise

get_parent_register modes reg | elem CsMode32 modes =
  case get_parent_register [CsMode16] reg of
    X86RegAx -> X86RegEax
    X86RegBx -> X86RegEbx
    X86RegCx -> X86RegEcx
    X86RegDx -> X86RegEdx
    X86RegBp -> X86RegEbp
    X86RegSi -> X86RegEsi
    X86RegDi -> X86RegEdi
    X86RegSp -> X86RegEsp
    X86RegR8w -> X86RegR8d
    X86RegR9w -> X86RegR9d
    X86RegR10w -> X86RegR10d
    X86RegR11w -> X86RegR11d
    X86RegR12w -> X86RegR12d
    X86RegR13w -> X86RegR13d
    X86RegR14w -> X86RegR14d
    X86RegR15w -> X86RegR15d
    otherwise -> otherwise

get_parent_register modes reg | elem CsMode64 modes =
  case get_parent_register [CsMode32] reg of
    X86RegEax -> X86RegRax
    X86RegEbx -> X86RegRbx
    X86RegEcx -> X86RegRcx
    X86RegEdx -> X86RegRdx
    X86RegEbp -> X86RegRbp
    X86RegEsi -> X86RegRsi
    X86RegEdi -> X86RegRdi
    X86RegEsp -> X86RegRsp
    X86RegR8d -> X86RegR8
    X86RegR9d -> X86RegR9
    X86RegR10d -> X86RegR10
    X86RegR11d -> X86RegR11
    X86RegR12d -> X86RegR12
    X86RegR13d -> X86RegR13
    X86RegR14d -> X86RegR14
    X86RegR15d -> X86RegR15
    otherwise -> otherwise

-- Checks if the given register is a segment register

is_segment_reg :: X86.X86Reg -> Bool
is_segment_reg reg = case reg of
  X86RegCs -> True
  X86RegDs -> True
  X86RegSs -> True
  X86RegEs -> True
  X86RegFs -> True
  X86RegGs -> True
  _ -> False

-- Checks if the given register is a control register

is_control_reg :: X86.X86Reg -> Bool
is_control_reg reg = case reg of
  X86RegCr0 -> True
  X86RegCr1 -> True
  X86RegCr2 -> True
  X86RegCr3 -> True
  X86RegCr4 -> True
  X86RegCr5 -> True
  X86RegCr6 -> True
  X86RegCr7 -> True
  X86RegCr8 -> True
  X86RegCr9 -> True
  X86RegCr10 -> True
  X86RegCr11 -> True
  X86RegCr12 -> True
  X86RegCr13 -> True
  X86RegCr14 -> True
  X86RegCr15 -> True
  _ -> False

-- Make operation to set the zero flag to the value that it would have after some operation

zf_s :: AstNode -> CsX86Op -> AstNode
zf_s parent dst =
  let bv_size = (size dst) * 8 in
    IteNode
      (EqualNode (ExtractNode (bv_size - 1) 0 parent) (BvNode 0 bv_size))
      (BvNode 1 1)
      (BvNode 0 1)

-- Make operation to set the overflow flag to the value that it would have after an add operation

of_add_s :: AstNode -> CsX86Op -> AstNode -> AstNode -> AstNode
of_add_s parent dst op1ast op2ast =
  let bv_size = (size dst) * 8 in
    ExtractNode (bv_size - 1) (bv_size - 1)
      (BvandNode
        (BvxorNode op1ast (BvnotNode op2ast))
        (BvxorNode op1ast (ExtractNode (bv_size - 1) 0 parent)))

-- Make operation to set the carry flag to the value that it would have after an add operation

cf_add_s :: AstNode -> CsX86Op -> AstNode -> AstNode -> AstNode
cf_add_s parent dst op1ast op2ast =
  let bv_size = (size dst) * 8 in
    ExtractNode (bv_size - 1) (bv_size - 1)
      (BvxorNode (BvandNode op1ast op2ast)
        (BvandNode (BvxorNode
            (BvxorNode op1ast op2ast)
            (ExtractNode (bv_size - 1) 0 parent))
          (BvxorNode op1ast op2ast)))

-- Make operation to set the adjust flag to the value that it would have after some operation

af_s :: AstNode -> CsX86Op -> AstNode -> AstNode -> AstNode
af_s parent dst op1ast op2ast =
  let bv_size = (size dst) * 8 in
    IteNode
      (EqualNode
        (BvNode 0x10 bv_size)
        (BvandNode
          (BvNode 0x10 bv_size)
          (BvxorNode
            (ExtractNode (bv_size - 1) 0 parent)
            (BvxorNode op1ast op2ast))))
      (BvNode 1 1)
      (BvNode 0 1)

-- Make operation to set the parity flag to the value that it would have after some operation

pf_s :: AstNode -> CsX86Op -> AstNode
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
    loop 0

-- Make operation to set the sign flag to the value that it would have after some operation

sf_s :: AstNode -> CsX86Op -> AstNode
sf_s parent dst =
  let bv_size = (size dst) * 8 in
    (ExtractNode (bv_size - 1) (bv_size - 1) parent)

-- Make list of operations in the IR that has the same semantics as the X86 add instruction

add_s :: CsInsn -> [Stmt]
add_s inst =
  let (op1 : op2 : _ ) = x86operands inst
      op1ast = getOperandAst op1
      op2ast = getOperandAst op2
      add_node = (BvaddNode op1ast op2ast)
  in [
      store_node (value op1) add_node,
      SetFlag Adjust (af_s add_node op1 op1ast op1ast),
      SetFlag Parity (pf_s add_node op1),
      SetFlag Sign (sf_s add_node op1),
      SetFlag Zero (zf_s add_node op1),
      SetFlag Carry (cf_add_s add_node op1 op1ast op1ast),
      SetFlag Overflow (of_add_s add_node op1 op1ast op1ast)
    ]

-- Make operation to set the carry flag to the value that it would have after an sub operation

cf_sub_s :: AstNode -> CsX86Op -> AstNode -> AstNode -> AstNode
cf_sub_s parent dst op1ast op2ast =
  let bv_size = (size dst) * 8 in
    ExtractNode (bv_size - 1) (bv_size - 1)
      (BvxorNode
        (BvxorNode op1ast (BvxorNode op2ast (ExtractNode (bv_size - 1) 0 parent)))
        (BvandNode
          (BvxorNode op1ast (ExtractNode (bv_size - 1) 0 parent))
          (BvxorNode op1ast op2ast)))

-- Make operation to set the overflow flag to the value that it would have after an sub operation

of_sub_s :: AstNode -> CsX86Op -> AstNode -> AstNode -> AstNode
of_sub_s parent dst op1ast op2ast =
  let bv_size = (size dst) * 8 in
    ExtractNode (bv_size - 1) (bv_size - 1)
      (BvandNode
        (BvxorNode op1ast op2ast)
        (BvxorNode op1ast (ExtractNode (bv_size - 1) 0 parent)))

-- Make list of operations in the IR that has the same semantics as the X86 sub instruction

sub_s :: CsInsn -> [Stmt]
sub_s inst =
  let (op1 : op2 : _ ) = x86operands inst
      op1ast = getOperandAst op1
      op2ast = getOperandAst op2
      sub_node = (BvsubNode op1ast op2ast)
  in [
      store_node (value op1) sub_node,
      SetFlag Adjust (af_s sub_node op1 op1ast op1ast),
      SetFlag Parity (pf_s sub_node op1),
      SetFlag Sign (sf_s sub_node op1),
      SetFlag Zero (zf_s sub_node op1),
      SetFlag Carry (cf_sub_s sub_node op1 op1ast op1ast),
      SetFlag Overflow (of_sub_s sub_node op1 op1ast op1ast)
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
      SetFlag Adjust UndefinedNode,
      SetFlag Parity (pf_s xor_node dst_op),
      SetFlag Sign (sf_s xor_node dst_op),
      SetFlag Zero (zf_s xor_node dst_op),
      SetFlag Carry (BvNode 0 1),
      SetFlag Overflow (BvNode 0 1)
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
      SetFlag Adjust UndefinedNode,
      SetFlag Parity (pf_s and_node dst_op),
      SetFlag Sign (sf_s and_node dst_op),
      SetFlag Zero (zf_s and_node dst_op),
      SetFlag Carry (BvNode 0 1),
      SetFlag Overflow (BvNode 0 1)
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
      SetFlag Adjust UndefinedNode,
      SetFlag Parity (pf_s and_node dst_op),
      SetFlag Sign (sf_s and_node dst_op),
      SetFlag Zero (zf_s and_node dst_op),
      SetFlag Carry (BvNode 0 1),
      SetFlag Overflow (BvNode 0 1)
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
        (Mem mem_struct) | get_parent_register modes (base mem_struct) == sp -> True
        _ -> False
      -- Is the destination register is SP?
      sp_reg = case (value op1) of
        (Reg reg) | get_parent_register modes reg == sp -> True
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
        [SetFlag Adjust UndefinedNode,
        SetFlag Parity UndefinedNode,
        SetFlag Sign UndefinedNode,
        SetFlag Zero UndefinedNode,
        SetFlag Carry UndefinedNode,
        SetFlag Overflow UndefinedNode]

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

get_stack_reg :: [CsMode] -> X86Reg
get_stack_reg modes =
  if elem CsMode32 modes then X86RegEsp
  else if elem CsMode32 modes then X86RegRsp
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
    (Reg reg) -> SetReg reg store_what
    (Mem mem) -> Store (getLeaAst mem) store_what
    _ -> error "Target of store operation is neither a register nor a memory operand."
    
