module Main where
import System.IO
import Lifter
--import Simplify

main :: IO ()
main = do
  let input = [0xB8, 0x01, 0x00, 0x00, 0x00, 0xB8, 0x02, 0x00, 0x00, 0x00]
  asm <- disasm_buf input
  case asm of
    Left _ -> print "error"
    Right b -> print (liftAsm b)
  -- let simplification = Simplify { buffer = first_pass }

  -- state <- lift_next_block $ new_state contents
  --compile_blocks $ toAscList $ blocks state
  -- print $ keys $ blocks state
  --print $ blocks_queue state
