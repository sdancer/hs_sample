module Main where

import System.IO
import Hapstone.Internal.Capstone as Capstone
--import EvalAst
import Ast
import SymbolicEval
import BitVector
import Phasses

import Lifter
--import Simplify


main :: IO ()
main = do
  print "this should be in test/"
  -- Ignoring the changes to the flag registers, the following is what happens

  let input = [0xBC, 0x64, 0x00, 0x00, 0x00, 0x50, 0xB8, 0x02, 0x00, 0x00, 0x00, 0x83, 0xC0, 0x0A, 0x58, 0x83, 0xE8, 0x03]
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
  let labelled = snd $ labelStmts 0 lifted
  -- Simplify the labelled statements by doing constant propagation and folding, and by
  -- introducing cross references
  let simplified = snd $ symExec (basicX86Context modes) labelled
  -- Elimate the dead code under the assumption that the flag bits are defined-before-use
  -- in the fragment of code that follows simplified
  let eliminated = snd $ eliminateDeadCode [(1408,1472)] simplified
  -- Now print what we have.
  print eliminated
  --print {-(getRegisterValues (reg_file (fst-} (symExec (basicX86Context modes) (snd $ labelStmts 0 lifted)){-)))-}
