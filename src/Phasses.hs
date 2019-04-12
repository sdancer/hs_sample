module Phasses where

import Data.List
import Ast
import Data.Maybe

data SG = Set Int Int | Get Int deriving (Show, Eq)

naive_dead_code_elimination =
  let folder ele (acc1, acc2) =
        let cond_add a = case (elem a acc2) of
                      True -> (acc1, acc2) -- skipped
                      False -> ([ele] ++ acc1, acc2 ++ [a])
            nacc2_delete a = delete a acc2
        in  case ele of
              Set a _ -> cond_add a
              Get a -> ([ele] ++ acc1, nacc2_delete a)
  in  foldr folder ([], []) [Set 5 1, Set 1 1, Set 1 2, Get 2, Get 1, Get 3, Set 1 3]

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

{-
fails on:
set 1 1
cond [ set 1 2 ]
get 1
-}
