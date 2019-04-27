module Main where

import System.IO
import Hapstone.Internal.Capstone as Capstone
--import EvalAst
import Ast
import BitVector
import Data.SBV
import Control.Monad.State.Lazy
import Phasses
import Lifter
--import Simplify

main :: IO ()
main = do
  let input = [0xB8, 0x00, 0x00, 0x00, 0x00, -- mov eax,0x0 becomes SetReg (0,32) (BvExpr 0 32)
                  0x83, 0xC0, 0x0A, -- add eax,0xa becomes SetReg (0,32) (BvExpr 10 32)
                  0x83, 0xC0, 0x0F, -- add eax,0xf becomes SetReg (0,32) (BvExpr 25 32)
                  0x83, 0xE8, 0x03, -- sub eax,0x3 becomes SetReg (0,32) (BvExpr 22 32)
                  0x89, 0xD8, -- mov eax,ebx becomes SetReg (0,32) (GetReg (64,96))
                  0x83, 0xC0, 0x14]
      modes = [Capstone.CsMode32]
  irStmts <- decompile modes (allMemory modes) input
  -- Now print the result of the above transformations.
  print irStmts

