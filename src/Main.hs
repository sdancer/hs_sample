module Main where

import qualified Data.ByteString            as BS
import           System.IO
import           Hapstone.Internal.Capstone as Capstone

import Lifter
--import Simplify

main :: IO ()
main = do
  -- contents <- BS.readFile "bs/blackcipher.aes"
  print "this should be in test/"
  let input = [0x83, 0xE8, 0x05]
  let modes = [Capstone.CsMode32]
  asm <- disasm_buf modes input
  case asm of
    Left _ -> print "error"
    Right b -> print (liftAsm modes b)
  -- let simplification = Simplify { buffer = first_pass }

  -- state <- lift_next_block $ new_state contents
  --compile_blocks $ toAscList $ blocks state
  -- print $ keys $ blocks state
  --print $ blocks_queue state
