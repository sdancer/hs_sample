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
  let input = [0xBC, 0x64, 0x00, 0x00, 0x00, 0x6A, 0x0A, 0x6A, 0x0D, 0x5B]
  let modes = [Capstone.CsMode32]
  asm <- disasm_buf modes input
  case asm of
    Left _ -> print "error"
    Right b -> print (getRegisterValues (reg_file (run x86Context (concat (liftAsm modes b)))))
  -- let simplification = Simplify { buffer = first_pass }

  -- state <- lift_next_block $ new_state contents
  --compile_blocks $ toAscList $ blocks state
  -- print $ keys $ blocks state
  --print $ blocks_queue state
