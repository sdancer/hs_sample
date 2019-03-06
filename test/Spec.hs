module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Lifter
import Ast
import Hapstone.Internal.X86

import Data.Word


safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x


main :: IO ()
main = defaultMain $
  testGroup "Lifter" [
    test_lift
    , test_simplify
  ]


test_lift =
  testCase "simple lift buf" $ do
    l <- liftX86toAst [0xB8, 0x01, 0x00, 0x00, 0x00, 0xB8, 0x02, 0x00, 0x00, 0x00]
    l @?= [
            [SetReg (X86Reg X86RegEax) (BvNode 1 32)]
            ,[SetReg (X86Reg X86RegEax) (BvNode 2 32)]
          ]

test_simplify =
  testCase "asm: \n mov eax, 1 \n add eax, 5 \n sub eax, 3" $ (do
        l <- liftX86toAst [0xB8, 0x01, 0x00, 0x00, 0x00, 0x83, 0xC0, 0x05, 0x83, 0xE8, 0x03]
        -- filter out flags
        l @?= [[SetReg (X86Reg X86RegEax) (BvNode 3 32)]]
      )
