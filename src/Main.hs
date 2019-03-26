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
  let input = [0xb8, 0x0a, 0x00, 0x00, 0x00, -- mov eax,0xa
              -- 00000005 <loop>:
              0x83, 0xc0, 0x0a, -- add eax,0xa
              0xeb, 0xfb] -- jmp loop
  let modes = [Capstone.CsMode32]
  asm <- disasm_buf modes input
  case asm of
    Left _ -> print "error"
    -- Register eax will be some multiple of 10
    Right b -> print ((iter symStep 2) (basicX86Context modes (liftAsm modes b)))

