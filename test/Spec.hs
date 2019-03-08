module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Lifter
import Ast
import Hapstone.Internal.X86

import Data.Word

import AstTools


safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x


main :: IO ()
main = defaultMain $
  testGroup "Lifter" [
    test_lift
    , test_simplify
    , test_eval
  ]


test_lift =
  testCase "simple lift buf" $ do
    l <- liftX86toAst [0xB8, 0x01, 0x00, 0x00, 0x00, 0xB8, 0x02, 0x00, 0x00, 0x00]
    l @?= [
            [SetReg (X86Reg X86RegEax) (BvNode 1 32)]
            ,[SetReg (X86Reg X86RegEax) (BvNode 2 32)]
          ]

test_simplify =
  testCase "asm: \n mov eax, 1 \n add eax, 5 \n sub eax, 3" $ do
        l1 <- liftX86toAst [0xB8, 0x01, 0x00, 0x00, 0x00, 0x83, 0xC0, 0x05, 0x83, 0xE8, 0x03]
        (filter_out_flags l1) @?= [
          [SetReg (X86Reg X86RegEax) (BvNode 1 32)],
          [SetReg (X86Reg X86RegEax) (BvaddNode (GetReg (X86Reg X86RegEax)) (BvNode 5 32))],
          [AssertNode "sub"]
          ]

-- test_eval =
--   testCase "eval" $ do
--         l1 <- liftX86toAst [0xB8, 0x01, 0x00, 0x00, 0x00, 0x83, 0xC0, 0x05, 0x83, 0xE8, 0x03]
--         --after running this code on a symbolic context, eax == 3
--         (filter_out_flags l1) @?= [
--           [SetReg (X86Reg X86RegEax) (BvNode 1 32)],
--           [SetReg (X86Reg X86RegEax) (BvaddNode (GetReg (X86Reg X86RegEax)) (BvNode 5 32))],
--           [AssertNode "sub"]
--           ]
--

test_eval =
  testCase "eval" $ do
        l1 <- liftX86toAst someasm
        --after running this code on a symbolic context, eax == 3
        (filter_out_flags l1) @?= [
          [SetReg (X86Reg X86RegEax) (BvNode 1 32)],
          [SetReg (X86Reg X86RegEax) (BvaddNode (GetReg (X86Reg X86RegEax)) (BvNode 5 32))],
          [AssertNode "sub"]
          ]
        where
          someasm = [87, 199, 4, 36, 250, 119, 87, 82, 129, 4, 36, 163, 203, 238, 15, 129, 4,
                  36, 193, 50, 251, 36, 129, 52, 36, 190, 74, 190, 111, 247, 28, 36, 129, 4,
                  36, 214, 232, 242, 40, 131, 236, 4, 137, 4, 36, 184, 245, 175, 243, 63, 49,
                  68, 36, 4, 88, 81, 199, 4, 36, 213, 199, 247, 127, 247, 20, 36, 129, 52, 36,
                  53, 123, 120, 89, 193, 36, 36, 1, 135, 60, 36, 247, 215, 135, 60, 36, 129, 4,
                   36, 63, 11, 156, 238, 193, 36, 36, 8, 87, 191, 182, 41, 204, 186, 49, 124,
                   36, 4, 95, 81, 137, 4, 36, 199, 4, 36, 210, 201, 191, 43]
