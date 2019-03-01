module Jitter where

-- todo, nothing done yet

compile_blocks :: [(Word64, AsmBlock)] -> IO ()
compile_blocks [] = return ()
compile_blocks ((_, ilist):xs) = do
    compile_block ilist
    compile_blocks xs

compile_block :: AsmBlock -> IO ()
compile_block [] = return ()
compile_block ((addr, Skip insn) : xs) = do
  putStrLn $ ";skip\t -->\t" ++ insn_to_str insn
  compile_block xs
compile_block ((addr, Break insn) : xs) = do
  putStrLn $ ";break\t -->\t" ++ insn_to_str insn
  compile_block xs
compile_block ((_, Insn insn) : xs) = do
  putStrLn $ insn_to_str insn
  compile_block xs
