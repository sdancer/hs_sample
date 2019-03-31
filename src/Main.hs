module Main where

import System.IO
import Hapstone.Internal.Capstone as Capstone
import EvalAst
import Ast
import SymbolicEval

import Lifter
--import Simplify

data SG = Set Int | Get Int deriving (Show, Eq)

(let folder ele (acc1, acc2) =
      let nacc2_add = case (elem ele acc2) of
                    True -> (acc1, acc2)
                    False -> (acc1 ++ [ele], )
          nacc2_delete = delete ele acc2
      in  case ele of
            Set a -> cond_add
            Get a -> (acc1 ++ [ele], nacc2_delete)
in  foldr folder ([], []) [Set 1, Get 2, Get 3, Set 1])


main :: IO ()
main = do
  print "this should be in test/"
  -- Ignoring the changes to the flag registers, the following is what happens
  let input = [0xBC, 0x00, 0x00, 0x00, 0x80, 0xB8, 0x37, 0x13, 0x22, 0x00, 0x50, 0xB8, 0x01, 0x00, 0x00, 0x00, 0x58, 0xB8, 0x02, 0x00, 0x00, 0x00]
      modes = [Capstone.CsMode32]
  asm <- disasm_buf modes input
  let lifted = case asm of
                  Left _ -> error "error on disasm"
                  Right b -> liftAsm modes b
      oneblock = foldl (\y (a, b) -> y ++ b) [] lifted
  print lifted
  print oneblock
  print (symSteps (basicX86Context modes lifted))

-- reduce from right to left
-- on set
--   if on acc, delete(skip) expression
--   else add to acc
-- on get, del from acc
-- gotta sumarize all gets a expression has
