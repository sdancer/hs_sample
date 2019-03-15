module X86Sem where

import Ast
import AstContext (getOperandAst)

import           Hapstone.Capstone
import           Hapstone.Internal.Capstone as Capstone
import           Hapstone.Internal.X86      as X86
import           Util
import           AstContext
import           Data.Maybe

--ll, ml, hl

-- Make operation to set the zero flag to the value that it would have after some operation

zf_s :: CsInsn -> AstNodeType -> CsX86Op -> AstNodeType
zf_s inst parent dst =
  let bv_size = (size dst) * 8 in
    IteNode
      (EqualNode (ExtractNode (bv_size - 1) 0 parent) (BvNode 0 bv_size))
      (BvNode 1 1)
      (BvNode 0 1)

-- Make operation to set the overflow flag to the value that it would have after an add operation

of_add_s :: CsInsn -> AstNodeType -> CsX86Op -> AstNodeType -> AstNodeType -> AstNodeType
of_add_s inst parent dst op1ast op2ast =
  let bv_size = (size dst) * 8 in
    ExtractNode (bv_size - 1) (bv_size - 1)
      (BvandNode
        (BvxorNode op1ast (BvnotNode op2ast))
        (BvxorNode op1ast (ExtractNode (bv_size - 1) 0 parent)))

-- Make operation to set the carry flag to the value that it would have after an add operation

cf_add_s :: CsInsn -> AstNodeType -> CsX86Op -> AstNodeType -> AstNodeType -> AstNodeType
cf_add_s inst parent dst op1ast op2ast =
  let bv_size = (size dst) * 8 in
    ExtractNode (bv_size - 1) (bv_size - 1)
      (BvxorNode (BvandNode op1ast op2ast)
        (BvandNode (BvxorNode
            (BvxorNode op1ast op2ast)
            (ExtractNode (bv_size - 1) 0 parent))
          (BvxorNode op1ast op2ast)))

-- Make operation to set the adjust flag to the value that it would have after some operation

af_s :: CsInsn -> AstNodeType -> CsX86Op -> AstNodeType -> AstNodeType -> AstNodeType
af_s inst parent dst op1ast op2ast =
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

byte_size_bit = 8

-- Make operation to set the parity flag to the value that it would have after some operation

pf_s :: CsInsn -> AstNodeType -> CsX86Op -> AstNodeType
pf_s inst parent dst =
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

sf_s :: CsInsn -> AstNodeType -> CsX86Op -> AstNodeType
sf_s inst parent dst =
  let bv_size = (size dst) * 8 in
    (ExtractNode (bv_size - 1) (bv_size - 1) parent)

-- Make list of operations in the IR that has the same semantics as the X86 add instruction

add_s :: CsInsn -> [AstNodeType]
add_s inst =
  let (op1 : op2 : _ ) = x86operands inst
      op1ast = getOperandAst op1
      op2ast = getOperandAst op2
      add_node = (BvaddNode op1ast op2ast)
  in [
      store_node (value op1) add_node,
      SetFlag Adjust (af_s inst add_node op1 op1ast op1ast),
      SetFlag Parity (pf_s inst add_node op1),
      SetFlag Sign (sf_s inst add_node op1),
      SetFlag Zero (zf_s inst add_node op1),
      SetFlag Carry (cf_add_s inst add_node op1 op1ast op1ast),
      SetFlag Overflow (of_add_s inst add_node op1 op1ast op1ast)
    ]

-- Make operation to set the carry flag to the value that it would have after an sub operation

cf_sub_s :: CsInsn -> AstNodeType -> CsX86Op -> AstNodeType -> AstNodeType -> AstNodeType
cf_sub_s inst parent dst op1ast op2ast =
  let bv_size = (size dst) * 8 in
    ExtractNode (bv_size - 1) (bv_size - 1)
      (BvxorNode
        (BvxorNode op1ast (BvxorNode op2ast (ExtractNode (bv_size - 1) 0 parent)))
        (BvandNode
          (BvxorNode op1ast (ExtractNode (bv_size - 1) 0 parent))
          (BvxorNode op1ast op2ast)))

-- Make operation to set the overflow flag to the value that it would have after an sub operation

of_sub_s :: CsInsn -> AstNodeType -> CsX86Op -> AstNodeType -> AstNodeType -> AstNodeType
of_sub_s inst parent dst op1ast op2ast =
  let bv_size = (size dst) * 8 in
    ExtractNode (bv_size - 1) (bv_size - 1)
      (BvandNode
        (BvxorNode op1ast op2ast)
        (BvxorNode op1ast (ExtractNode (bv_size - 1) 0 parent)))

-- Make list of operations in the IR that has the same semantics as the X86 sub instruction

sub_s :: CsInsn -> [AstNodeType]
sub_s inst =
  let (op1 : op2 : _ ) = x86operands inst
      op1ast = getOperandAst op1
      op2ast = getOperandAst op2
      sub_node = (BvsubNode op1ast op2ast)
  in [
      store_node (value op1) sub_node,
      SetFlag Adjust (af_s inst sub_node op1 op1ast op1ast),
      SetFlag Parity (pf_s inst sub_node op1),
      SetFlag Sign (sf_s inst sub_node op1),
      SetFlag Zero (zf_s inst sub_node op1),
      SetFlag Carry (cf_sub_s inst sub_node op1 op1ast op1ast),
      SetFlag Overflow (of_sub_s inst sub_node op1 op1ast op1ast)
    ]

-- Make list of operations in the IR that has the same semantics as the X86 xor instruction

xor_s :: CsInsn -> [AstNodeType]
xor_s inst =
  let (op1 : op2 : _ ) = x86operands inst
      op1ast = getOperandAst op1
      op2ast = getOperandAst op2
      xor_node = (BvxorNode op1ast op2ast)
  in [
      store_node (value op1) xor_node,
      SetFlag Adjust UndefinedNode,
      SetFlag Parity (pf_s inst xor_node op1),
      SetFlag Sign (sf_s inst xor_node op1),
      SetFlag Zero (zf_s inst xor_node op1),
      SetFlag Carry (BvNode 0 1),
      SetFlag Overflow (BvNode 0 1)
    ]

push ::  CsInsn -> [AstNodeType]
push inst =
  let (op1 : _) = x86operands inst
      op1ast = getOperandAst op1
  in [
      SetReg stack_register (BvsubNode (GetReg stack_register) (BvNode 4 32)),
      Store (GetReg stack_register) op1ast
    ]

pop ::  CsInsn -> [AstNodeType]
pop inst =
  --whenever the operation is a store reg or store mem depends on op1
  let
    read_exp = Read (BvaddNode (GetReg stack_register) (BvNode 4 32))
  in
   [
      SetReg stack_register (BvaddNode (GetReg stack_register) (BvNode 4 32)),
      store_node (get_first_opr_value inst) read_exp
    ]

mov ::  CsInsn -> [AstNodeType]
mov inst =
  let
    (op1 : op2 : _ ) = x86operands inst
  in
    [store_node (value op1) (getOperandAst op2)]


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

--isolate x86 32 bit specific stuff so its easier to refactor later
stack_register :: Register
stack_register = (X86Reg X86RegEsp)

--byte size is ignored
store_node :: CsX86OpValue -> AstNodeType -> AstNodeType
store_node operand store_what =
            case operand of
              (Reg reg) -> (SetReg (X86Reg reg) store_what)
              (Mem mem) -> Store (getLeaAst mem) store_what
              (Imm _) -> AssertNode "store to imm, wtf"
