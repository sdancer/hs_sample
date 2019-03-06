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
  testGroup "Lifter" [ test_simple_lift ]

test_simple_lift =
  testCase "simple lift buf" $ do
    l <- liftX86 [0xB8, 0x01, 0x00, 0x00, 0x00, 0xB8, 0x02, 0x00, 0x00, 0x00]
    l @?= [[SetReg (X86Reg X86RegEax) (BvNode 1 32)],[SetReg (X86Reg X86RegEax) (BvNode 2 32)]]

liftX86 :: [Word8] -> IO [[AstNodeType]]
liftX86 input = do
    asm <- disasm_buf input
    return (case asm of
      Left _ -> [[(AssertNode "dissasm error")]]
      Right b -> liftAsm b)
