module X86Sem where

import Ast
import AstContext (getOperandAst)

import           Hapstone.Capstone
import           Hapstone.Internal.Capstone as Capstone
import           Hapstone.Internal.X86      as X86
import           Util

--ll, ml, hl

add_s :: CsInsn -> [AstNodeType]
add_s inst =
  let op1 = getOperandAst $ get_first_opr_value inst
      op2 = getOperandAst $ get_second_opr_value inst
  in [
      BvaddNode op1 op2,
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
      BvsubNode op1 op2,
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
      BvxorNode op1 op2,
      SetFlag Adjust (AssertNode "Adjust flag unimplemented"),
      SetFlag Parity (AssertNode "Parity flag unimplemented"),
      SetFlag Sign (AssertNode "Sign flag unimplemented"),
      SetFlag Zero (AssertNode "Zero flag unimplemented"),
      SetFlag Carry (AssertNode "Carry flag unimplemented"),
      SetFlag Overflow (AssertNode "Overflow flag unimplemented")
    ]
--
-- void x86Semantics::add_s(triton::arch::Instruction& inst) {
--   auto& dst = inst.operands[0];
--   auto& src = inst.operands[1];
--
--   /* Create symbolic operands */
--   auto op1 = this->symbolicEngine->getOperandAst(inst, dst);
--   auto op2 = this->symbolicEngine->getOperandAst(inst, src);
--
--   /* Create the semantics */
--   auto node = this->astCtxt.bvadd(op1, op2);
--
--   /* Create symbolic expression */
--   auto expr = this->symbolicEngine->createSymbolicExpression(inst, node, dst, "ADD operation");
--
--   /* Spread taint */
--   expr->isTainted = this->taintEngine->taintUnion(dst, src);
--
--   /* Update symbolic flags */
--   this->af_s(inst, expr, dst, op1, op2);
--   this->cfAdd_s(inst, expr, dst, op1, op2);
--   this->ofAdd_s(inst, expr, dst, op1, op2);
--   this->pf_s(inst, expr, dst);
--   this->sf_s(inst, expr, dst);
--   this->zf_s(inst, expr, dst);
--
--   /* Update the symbolic control flow */
--   this->controlFlow_s(inst);
-- }
