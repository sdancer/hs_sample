module X86Sem where

import Ast
import AstContext (getOperandAst)

--ll, ml, hl

add_s :: instr -> [AstNodeType]
add_s inst =
  let op1 = getOperandAst(inst dst)
      op2 = getOperandAst(inst src)
  in [BvaddNode op1 op2]

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
