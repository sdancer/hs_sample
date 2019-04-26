module Main where

import System.IO
import Hapstone.Internal.Capstone as Capstone
import EvalAst
import Ast
import SymbolicEval
import BitVector
import Phasses
import Data.Maybe
import Data.SBV

import Lifter
--import Simplify


main :: IO ()
main = do
  print "this should be in test/"
  -- Ignoring the changes to the flag registers, the following is what happens

  let input = [0x51, 0xB9, 0x13, 0x8F, 0xC5, 0x77, 0x81, 0xC9, 0xDF, 0x95, 0x9E, 0x0F, 0x81, 0xC1, 0x0D, 0x18, 0x08, 0xE0, 0x01, 0xCB, 0x59]
      modes = [Capstone.CsMode32]
  asm <- disasm_buf modes input
  let lifted = case asm of
                  Left _ -> error "error on disasm"
                  Right b -> liftAsm modes b
      oneblock = lifted
  -- print lifted
  -- print oneblock
  
  -- Label the statements produced by lifting from assembly. The labels are necessary for
  -- the cross referencing that happens in the next stage.
  let labelled = snd $ labelStmts 0 (Compound undefined $ lifted)
  -- Simplify the labelled statements by doing constant propagation and folding.
  simplified <- symExec (SymbolicEval.basicX86Context modes) labelled
  -- Eliminate the dead SetRegs under the assumption that the flag bits are defined-before-use
  -- in the fragment of code that follows simplified. Wrap it in a statement.
  let srEliminated = Compound (-1) $ maybeToList $ snd $ eliminateDeadSetRegs [(1408,1472)] (snd simplified)
  -- Eliminate the dead Stores. Wrap it in a statement.
  sEliminated <- Compound (-1) <$> maybeToList <$> snd <$> eliminateDeadStores [] srEliminated
  -- Now introduce cross references into the statements. This must be done after dead code
  -- elimination as it obscures the locations where expressions are loaded from storage.
  referenced <- insertRefs (SymbolicEval.basicX86Context modes) sEliminated
  -- Now print the result of the above transformations.
  print (absToIdStmt $ snd referenced)
  -- equality <- exprEquals (BvaddExpr (BvExpr (intToBv 2 32)) (GetReg (0,32))) (BvaddExpr (GetReg (0,32)) (BvExpr (intToBv 2 32)))
  -- print equality
  --print {-(getRegisterValues (reg_file (fst-} (symExec (basicX86Context modes) (snd $ labelStmts 0 lifted)){-)))-}
