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

add_s :: CsInsn -> [AstNodeType]
add_s inst =
  let op1 = getOperandAst $ get_first_opr_value inst
      op2 = getOperandAst $ get_second_opr_value inst
  in [
      store_node (get_first_opr_value inst) (BvaddNode op1 op2),
      SetFlag Adjust (AssertNode "Adjust flag unimplemented"),
      SetFlag Parity (AssertNode "Parity flag unimplemented"),
      SetFlag Sign (AssertNode "Sign flag unimplemented"),
      SetFlag Zero (AssertNode "Zero flag unimplemented"),
      SetFlag Carry (AssertNode "Carry flag unimplemented"),
      SetFlag Overflow (AssertNode "Overflow flag unimplemented")
    ]

sub_s :: CsInsn -> [AstNodeType]
sub_s inst =
  let op1 = getOperandAst $ get_first_opr_value inst
      op2 = getOperandAst $ get_second_opr_value inst
  in [
      store_node (get_first_opr_value inst) (BvsubNode op1 op2),
      SetFlag Adjust (AssertNode "Adjust flag unimplemented"),
      SetFlag Parity (AssertNode "Parity flag unimplemented"),
      SetFlag Sign (AssertNode "Sign flag unimplemented"),
      SetFlag Zero (AssertNode "Zero flag unimplemented"),
      SetFlag Carry (AssertNode "Carry flag unimplemented"),
      SetFlag Overflow (AssertNode "Overflow flag unimplemented")
    ]

xor_s :: CsInsn -> [AstNodeType]
xor_s inst =
  let op1 = getOperandAst $ get_first_opr_value inst
      op2 = getOperandAst $ get_second_opr_value inst
  in [
      store_node (get_first_opr_value inst) (BvxorNode op1 op2),
      SetFlag Adjust (AssertNode "Adjust flag unimplemented"),
      SetFlag Parity (AssertNode "Parity flag unimplemented"),
      SetFlag Sign (AssertNode "Sign flag unimplemented"),
      SetFlag Zero (AssertNode "Zero flag unimplemented"),
      SetFlag Carry (AssertNode "Carry flag unimplemented"),
      SetFlag Overflow (AssertNode "Overflow flag unimplemented")
    ]

push ::  CsInsn -> [AstNodeType]
push inst =
  let op1 = getOperandAst $ get_first_opr_value inst
  in [
      SetReg stack_register (BvsubNode (GetReg stack_register) (BvNode 4 32)),
      Store (GetReg stack_register) op1
    ]

pop ::  CsInsn -> [AstNodeType]
pop inst =
  --whenever the operation is a store reg or store mem depends on op1
  let
    read_exp = Read (BvaddNode (GetReg stack_register) (BvNode 4 32))
    pop_op = case (get_first_opr_value inst) of
              (Reg reg) -> (SetReg stack_register read_exp)
              (Mem mem) -> Store (getLeaAst mem) read_exp
              (Imm _) -> AssertNode "pop with imm, wtf"
  in
   [
      SetReg stack_register (BvaddNode (GetReg stack_register) (BvNode 4 32)),
      pop_op
    ]

mov ::  CsInsn -> [AstNodeType]
mov inst =
  let
    (op1 : op2 : _) = x86operands inst
  in
    [store_node (value op1) (getOperandAst (value op2))]


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

--        x = maybe (Just d) (Capstone.detail insn)

-- return first operand in a instruction
-- insn_opr :: Int -> CsInsn -> Maybe CsX86Op
-- insn_opr i insn = case Capstone.detail insn of
--   Nothing -> Nothing
--   Just d -> case archInfo d of
--     Nothing        -> Nothing
--     Just (X86 ari) -> if length (operands ari) < i
--       then Nothing
--       else Just $ (operands ari) !! i
--
-- get_first_opr_value :: CsInsn -> CsX86OpValue
-- get_first_opr_value insn = case insn_opr 0 insn of
--   Nothing -> error ("nothing in first operand " ++ show insn)
--   Just op -> value op


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
