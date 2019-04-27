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
import Control.Monad.State.Lazy

import Lifter
--import Simplify

-- Takes the processor mode, writable address ranges, and executable code. Validates
-- memory accesses and returns a simplified Stmt in the IR that is semantically
-- equivalent.

decompile :: [CsMode] -> [(Expr, Expr)] -> [Word8] -> IO IdStmt

decompile modes writableMemory input = do
  asm <- disasm_buf modes input
  let lifted = case asm of
                  Left _ -> error "error on disasm"
                  Right b -> liftAsm modes b
  -- Label the statements produced by lifting from assembly. The labels are necessary for
  -- the cross referencing that happens in the next stage.
  let labelled = snd $ labelStmts 0 (Compound undefined $ lifted)
  -- Simplify the labelled statements by doing constant propagation and folding.
  simplified <- symExec (SymbolicEval.basicX86Context modes) labelled
  -- The program is only able to modify addresses that can be proven to be in writableMemory
  validateWrites writableMemory (snd simplified)
  -- Eliminate the dead SetRegs under the assumption that the flag bits are defined-before-use
  -- in the fragment of code that follows simplified. Wrap it in a statement.
  let srEliminated = Compound (-1) $ maybeToList $ snd $ eliminateDeadSetRegs [(1408,1472)] (snd simplified)
  -- Eliminate the dead Stores. Wrap it in a statement.
  sEliminated <- Compound (-1) <$> maybeToList <$> snd <$> eliminateDeadStores [] srEliminated
  -- Now introduce cross references into the statements. This must be done after dead code
  -- elimination as it obscures the locations where expressions are loaded from storage.
  referenced <- insertRefs (SymbolicEval.basicX86Context modes) sEliminated
  -- Now return the result of the above transformations.
  return (absToIdStmt $ snd referenced)

main :: IO ()
main = do
  let input = [0x6A, 0x0A, 0x6A, 0x0A, 0x6A, 0x0A, 0x6A, 0x0A]
      modes = [Capstone.CsMode32]
  irStmts <- decompile modes (allMemory modes) input
  -- Now print the result of the above transformations.
  print irStmts

