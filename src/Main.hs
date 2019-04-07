module Main where

import System.IO
import Hapstone.Internal.Capstone as Capstone
--import EvalAst
import Ast
import SymbolicEval
import BitVector

import Lifter
--import Simplify


main :: IO ()
main = do
  print "this should be in test/"
  -- Ignoring the changes to the flag registers, the following is what happens

  let input = [0xB8, 0x02, 0x00, 0x00, 0x00, 0xB8, 0x02, 0x00, 0x00, 0x00]
      modes = [Capstone.CsMode32]
  asm <- disasm_buf modes input
  let lifted = case asm of
                  Left _ -> error "error on disasm"
                  Right b -> liftAsm modes b
      oneblock = foldl (\y (a, b) -> y ++ b) [] lifted
  -- print lifted
  -- print oneblock
  print (getRegisterValues (reg_file (fst (symSteps (basicX86Context modes lifted)))))
