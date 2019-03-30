module Main where

import System.IO
import Hapstone.Internal.Capstone as Capstone
import EvalAst
import Ast
import SymbolicEval

import Lifter
--import Simplify

main :: IO ()
main = do
  print "this should be in test/"
  -- Ignoring the changes to the flag registers, the following is what happens
  let input = [0xBC, 0x00, 0x00, 0x00, 0x80, 0xB8, 0x37, 0x13, 0x22, 0x00, 0x50, 0xB8, 0x01, 0x00, 0x00, 0x00, 0x58]
      modes = [Capstone.CsMode32]
  asm <- disasm_buf modes input
  case asm of
    Left _ -> print "error"
    Right b -> print (symSteps (basicX86Context modes (liftAsm modes b)))
