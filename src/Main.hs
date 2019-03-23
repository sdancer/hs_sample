module Main where

import qualified Data.ByteString            as BS
import           System.IO
import           Hapstone.Internal.Capstone as Capstone
import EvalAst
import Ast

import Lifter
--import Simplify

main :: IO ()
main = do
  -- contents <- BS.readFile "bs/blackcipher.aes"
  print "this should be in test/"
  let input = [0xBC, 0x64, 0x00, 0x00, 0x00, -- mov esp,0x64
              0x6A, 0x0A, -- push 0xa
              0x6A, 0x0D, -- push 0xd
              0x5B, -- pop ebx
              0x59, -- pop ecx
              0x01, 0xCB] -- add ebx,ecx 
  let modes = [Capstone.CsMode32]
  asm <- disasm_buf modes input
  case asm of
    Left _ -> print "error"
    -- Register ebx will contain 23 as it is the result of 0xa+0xd
    Right b -> print (getRegisterValues (reg_file (run x86Context (concat (liftAsm modes b)))))






