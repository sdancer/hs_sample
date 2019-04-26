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

updateDefinedRegisters :: [CompoundReg] -> AbsStmt -> [CompoundReg]

updateDefinedRegisters regs (Comment _ _) = regs

updateDefinedRegisters regs (SetReg _ reg expr) =
  let removeAccessedReg rs e = case e of
        GetReg r -> removeRegister rs r
        Load a b -> foldl removeAccessedReg rs (flatten b)
        _ -> rs
  in foldl removeAccessedReg (addRegister regs reg) (flatten expr)

updateDefinedRegisters regs (Store _ _ expr) =
  let removeAccessedReg rs e = case e of
        GetReg r -> removeRegister rs r
        Load a b -> foldl removeAccessedReg rs (flatten b)
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

eliminateDeadSetRegs :: [CompoundReg] -> AbsStmt -> ([CompoundReg], Maybe AbsStmt)

eliminateDeadSetRegs regs (SetReg id reg expr) | isRegisterContained regs reg =
  (updateDefinedRegisters regs (SetReg id reg expr), Nothing)

eliminateDeadSetRegs regs (Compound id stmts) =
  let (finalRegs, newStmts) = mapAccumR eliminateDeadSetRegs regs stmts
  in (finalRegs, Just $ Compound id (catMaybes newStmts))

eliminateDeadSetRegs regs stmt = (updateDefinedRegisters regs stmt, Just stmt)

-- Removes the given expression and possibly equal expressions from the list.

removeExpr :: MonadIO m => [Expr] -> Expr -> m [Expr]

removeExpr [] _ = return []

removeExpr (c:d) e = do
  result <- exprEquals c e
  case result of
    Nothing -> removeExpr d e
    Just True -> removeExpr d e
    Just False -> do
      rmed <- removeExpr d e
      return (c:rmed)

-- Checks if the given expression is definitely contained in the given list

isExprContained :: MonadIO m => [Expr] -> Expr -> m Bool

isExprContained [] _ = return False

isExprContained (e:es) x = do
  result <- exprEquals e x
  case result of
    Nothing -> isExprContained es x
    Just False -> isExprContained es x
    Just True -> return True

-- If a statement sets a particular address, then that address is added to the
-- defined-before-use address list. If a statement accesses a particular address,
-- then that address is removed from the defined-before-use address list.

updateDefinedAddresses :: MonadIO m => [Expr] -> AbsStmt -> m [Expr]

updateDefinedAddresses addrs (Comment _ _) = return addrs

updateDefinedAddresses addrs (SetReg (id, valAbs) _ _) =
  let removeAccessedAddr as e = case e of
        Load a b -> do
          rmed <- removeExpr as b
          foldM removeAccessedAddr rmed (flatten b)
        _ -> return as
  in foldM removeAccessedAddr addrs (flatten valAbs)

updateDefinedAddresses addrs (Store (id, destAbs, valAbs) _ _) =
  let removeAccessedAddr as e = case e of
        Load a b -> do
          rmed <- removeExpr as b
          foldM removeAccessedAddr as (flatten b)
        _ -> return as
  in foldM removeAccessedAddr (destAbs:addrs) (flatten valAbs)

updateDefinedAddresses addrs (Compound _ stmts) =
  foldM updateDefinedAddresses addrs stmts

-- Eliminates dead code by keeping track of the addresses that are defined before use.
-- First argument is the list of addresses that have been defined before use in the
-- fragment after the supplied statement. Second argument is the statement whose liveness
-- needs to be determined. Returns the argument statement depending on whether or not it
-- is dead. Also returns the list of addresses that have been defined before use in the
-- fragment beginning with the supplied statement.

eliminateDeadStores :: MonadIO m => [Expr] -> AbsStmt -> m ([Expr], Maybe AbsStmt)

eliminateDeadStores addrs (Store (id, destAbs, valAbs) destRel valRel) = do
  contained <- isExprContained addrs destAbs
  let stmt = Store (id, destAbs, valAbs) destRel valRel
  newAddrs <- updateDefinedAddresses addrs stmt
  return (newAddrs, if contained then Nothing else Just stmt)

eliminateDeadStores addrs (Compound id stmts) = do
  (finalAddrs, newStmts) <- mapAccumRM eliminateDeadStores addrs stmts
  return (finalAddrs, Just $ Compound id (catMaybes newStmts))

eliminateDeadStores addrs stmt = do
  newAddrs <- updateDefinedAddresses addrs stmt
  return (newAddrs, Just stmt)

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

insertRefs :: MonadIO m => SymExecutionContext -> AbsStmt -> m (SymExecutionContext, AbsStmt)

insertRefs cin (SetReg (id, absVal) bs a) = do
  relVal <- simplifyExpr <$> substituteRel cin a
  return (cin { relativeRegisterFile = setRegisterValue (relativeRegisterFile cin) bs (toStaticExpr relVal id) },
        SetReg (id, absVal) bs relVal)

insertRefs cin (Store (id, pdestAbs, absVal) pdestRel pvalRel) = do
  pvalRel <- simplifyExpr <$> substituteRel cin pvalRel
  relativeMemoryV <- updateMemory (relativeMemory cin) (pdestAbs, toStaticExpr pvalRel id)
  return (cin { relativeMemory = relativeMemoryV }, Store (id, pdestAbs, absVal) pdestRel pvalRel)

insertRefs cin (Comment id str) = return (cin, Comment id str)

insertRefs cin (Compound id stmts) = do
  (i,s) <- mapAccumM insertRefs cin stmts
  return (i, Compound id s)

-- Labels all the statements in the instructions with a new unique identifier

labelStmts :: Int -> Stmt a b c d -> (Int, IdStmt)

labelStmts start (SetReg id bs a) = (start + 1, SetReg start bs a)

labelStmts start (Store id dst val) = (start + 1, Store start dst val)

labelStmts start (Comment id str) = (start + 1, Comment start str)

labelStmts start (Compound id stmts) = (i, Compound start s)
  where (i,s) = mapAccumL labelStmts (start + 1) stmts

-- Convert absolute statements to id statements

absToIdStmt :: AbsStmt -> IdStmt

absToIdStmt (SetReg (id, _) bs a) = SetReg id bs a

absToIdStmt (Store (id, _, _) dst val) = Store id dst val

absToIdStmt (Comment id str) = Comment id str

absToIdStmt (Compound id stmts) = Compound id (map absToIdStmt stmts)

