module Phasses where

import Data.List
import Ast
import Data.Maybe
import SymbolicEval
import BitVector
import Control.Monad.State.Lazy

-- If a statement sets a particular register, then that register is added to the
-- defined-before-use register list. If a statement accesses a particular register,
-- then that register is removed from the defined-before-use register list.

updateDefinedRegisters :: [CompoundReg] -> Stmt a -> [CompoundReg]

updateDefinedRegisters regs (SetReg _ reg expr) =
  let removeAccessedReg rs e = case e of
        GetReg r -> removeRegister rs r
        _ -> rs
  in foldl removeAccessedReg (addRegister regs reg) (flatten expr)

updateDefinedRegisters regs (Store _ _ expr) =
  let removeAccessedReg rs e = case e of
        GetReg r -> removeRegister rs r
        _ -> rs
  in foldl removeAccessedReg regs (flatten expr)

updateDefinedRegisters regs (Compound _ stmts) =
  foldl updateDefinedRegisters regs stmts

-- Eliminates dead code by keeping track of the registers that are defined before use.
-- First argument is the list of registers that have been defined before use in the
-- fragment after the supplied statement. Second argument is the statement whose liveness
-- needs to be determined. Returns the argument statement depending on whether or not it
-- is dead. Also returns the list of registers that have been defined before use in the
-- fragment beginning with the supplied statement.

eliminateDeadCode :: [CompoundReg] -> Stmt a -> ([CompoundReg], Maybe (Stmt a))

eliminateDeadCode regs (SetReg id reg expr) | isRegisterContained regs reg =
  (updateDefinedRegisters regs (SetReg id reg expr), Nothing)

eliminateDeadCode regs (Compound id stmts) =
  let (finalRegs, newStmts) = mapAccumR eliminateDeadCode regs stmts
  in (finalRegs, Just $ Compound id (catMaybes newStmts))

eliminateDeadCode regs stmt = (updateDefinedRegisters regs stmt, Just stmt)

-- A static expression is one whose value is not affected by a change in context. If the
-- input expression is a literal or a reference to some expression, then it is already
-- static. Otherwise return a reference to this expression.

toStaticExpr :: Expr -> Int -> Expr

toStaticExpr exprVal id = case exprVal of
  -- If it is a literal, put it in register as is
  BvExpr v -> BvExpr v
  -- If it is an expression reference, put it in register as is
  ReferenceExpr s v -> ReferenceExpr s v
  -- Otherwise put in a reference to this expression
  a -> ReferenceExpr (getExprSize a) id

-- Symbolically executes the statement on the given context, potentially simplifying it in
-- the process. Put the result of the simplification or a self-reference into storage.
-- Returns resulting context.

insertRefs :: MonadIO m => SymExecutionContext -> Stmt Int -> m (SymExecutionContext, Stmt Int)

insertRefs cin (SetReg id bs a) = do
  absVal <- simplifyExpr <$> substituteAbs cin a
  relVal <- simplifyExpr <$> substituteRel cin a
  return (cin { relativeRegisterFile = setRegisterValue (relativeRegisterFile cin) bs (toStaticExpr relVal id),
            absoluteRegisterFile = setRegisterValue (absoluteRegisterFile cin) bs absVal },
        SetReg id bs relVal)

insertRefs cin (Store id dst val) = do
  pdest <- simplifyExpr <$> substituteAbs cin dst
  pvalAbs <- simplifyExpr <$> substituteAbs cin val
  pvalRel <- simplifyExpr <$> substituteRel cin val
  relativeMemoryV <- updateMemory (relativeMemory cin) (pdest, toStaticExpr pvalRel id)
  absoluteMemoryV <- updateMemory (absoluteMemory cin) (pdest, pvalAbs)
  return (cin { relativeMemory = relativeMemoryV, absoluteMemory = absoluteMemoryV },
        Store id pdest pvalRel)

insertRefs cin (Comment str) = return (cin, Comment str)

insertRefs cin (Compound id stmts) = do
  (i,s) <- mapAccumM insertRefs cin stmts
  return (i, Compound id s)

{-
fails on:
set 1 1
cond [ set 1 2 ]
get 1
-}
