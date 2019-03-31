module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Lifter
import Ast
import Hapstone.Internal.X86
import Hapstone.Internal.Capstone as Capstone

import Data.Word

import AstTools
import SymbolicEval

import Lifter
import EvalAst

import TestBlockOne (test_block_one)

main :: IO ()
main = defaultMain $
  testGroup "Lifter" [
      test_lift
    , test_sym
    , test_block_one
  ]

test_lift =
  testCase "simple lift buf" $ do
    -- asm <- disasm_buf [Capstone.CsMode32] [0xB8, 0x01, 0x00, 0x00, 0x00, 0xB8, 0x02, 0x00, 0x00, 0x00]
    -- l <- liftAsm [Capstone.CsMode32] asm
    l <- liftX86toAst [Capstone.CsMode32] [0xB8, 0x01, 0x00, 0x00, 0x00, 0xB8, 0x02, 0x00, 0x00, 0x00]
    l @?= [(0,[SetReg (1472,1504) (BvExpr 5 4),SetReg (0,32) (BvExpr 1 32)]),
      (5,[SetReg (1472,1504) (BvExpr 10 4),SetReg (0,32) (BvExpr 2 32)])]


test_sym =
  testCase "symbolic" $ do
      let input = [0xB8, 0x00, 0x00, 0x00, 0x00, -- mov eax,0x0 becomes SetReg (0,32) (BvExpr 0 32)
                  0x83, 0xC0, 0x0A, -- add eax,0xa becomes SetReg (0,32) (BvExpr 10 32)
                  0x83, 0xC0, 0x0F, -- add eax,0xf becomes SetReg (0,32) (BvExpr 25 32)
                  0x83, 0xE8, 0x03, -- sub eax,0x3 becomes SetReg (0,32) (BvExpr 22 32)
                  0x89, 0xD8, -- mov eax,ebx becomes SetReg (0,32) (GetReg (64,96))
                  0x83, 0xC0, 0x14] -- add eax,0x14 becomes SetReg (0,32) (BvaddExpr (GetReg (0,32)) (BvExpr 20 32))
      l <- liftX86toAst [Capstone.CsMode32] input
      let context = basicX86Context [Capstone.CsMode32] l
      (symSteps context) @?= (context, [])
