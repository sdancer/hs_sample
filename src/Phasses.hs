module Phasses where

import Data.List
import Ast
import Data.Maybe
import SymbolicEval
import BitVector
import Control.Monad.State.Lazy
import Hapstone.Internal.Capstone as Capstone

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
  result <- proveRelation (EqualExpr c e)
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
  result <- proveRelation (EqualExpr e x)
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

-- Lookup the start inclusive, end exclusive range that contains the given Expr and the
-- n-1 bytes that follow it where n is the given integer.

lookupRange :: MonadIO m => [(Expr, Expr)] -> Expr -> Int -> m (Maybe (Expr, Expr))

lookupRange [] _ _ = return Nothing

lookupRange ((rangeStart, rangeEnd):rst) exprStart sz = do
  let exprEnd = BvaddExpr exprStart (BvExpr (toBv (div sz byte_size_bit) (getExprSize exprStart)))
  -- The subtractions in the following inequalities are necessary because we are doing
  -- modular arithmetic.
  result <- proveRelation (BvandExpr
    (BvuleExpr (BvsubExpr exprStart rangeStart) (BvsubExpr rangeEnd rangeStart))
    (BvuleExpr (BvsubExpr exprEnd rangeStart) (BvsubExpr rangeEnd rangeStart)))
  case result of
    Just True -> return (Just (rangeStart, rangeEnd))
    Just False -> lookupRange rst exprStart sz
    Nothing -> lookupRange rst exprStart sz

-- Ensures that the memory writes in the given statement fall within the given address
-- ranges.

validateWrites :: MonadIO m => [(Expr, Expr)] -> AbsStmt -> m ()

validateWrites ranges (Store (id, pdestAbs, absVal) pdestRel pvalRel) = do
  range <- lookupRange ranges pdestAbs (getExprSize pvalRel)
  case range of
    Just _ -> return ()
    Nothing -> fail ("Unable to prove that " ++ stmt ++ " falls within writable memory.")
                  where stmt = show (Store (id) pdestRel pvalRel :: IdStmt)

validateWrites ranges (Compound id stmts) = mapM_ (validateWrites ranges) stmts

validateWrites _ _ = return ()

-- The range that spans all memory. -1 when taken as an unsigned quantity is the largest
-- possible bit-vector because all its bits are set.

allMemory :: [CsMode] -> [(Expr, Expr)]

allMemory modes = [(BvExpr (toBv 0 archBitSize), BvExpr (toBv (-1) archBitSize))]
              where archBitSize = getArchBitSize modes

-- Convert absolute statements to id statements

absToIdStmt :: AbsStmt -> IdStmt

absToIdStmt (SetReg (id, _) bs a) = SetReg id bs a

absToIdStmt (Store (id, _, _) dst val) = Store id dst val

absToIdStmt (Comment id str) = Comment id str

absToIdStmt (Compound id stmts) = Compound id (map absToIdStmt stmts)

