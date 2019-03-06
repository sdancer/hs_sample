module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Lifter
import Ast

import Data.Word


safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x


main :: IO ()
main = defaultMain $
  testCase "Example test case" $ do
    -- assertion no. 1 (passes)
    2 + 2 @?= 4
    -- assertion no. 2 (fails)
    l <- liftX86 [0xB8, 0x01, 0x00, 0x00, 0x00, 0xB8, 0x02, 0x00, 0x00, 0x00]
    assertEqual "the list is not empty" [[(AssertNode "error")]] l
    -- assertion no. 3 (would have failed, but won't be executed because
    -- the previous assertion has already failed)
    l @?= [[SetReg (X86Reg X86RegEax) (BvNode 1 32)],[SetReg (X86Reg X86RegEax) (BvNode 2 32)]]

-- tests :: TestTree
-- tests = testGroup "Testing Examples" [unitTests]

-- unitTests :: TestTree
-- unitTests = testGroup "Tests (run via HUnit)" [ test_simple_lift ]

liftX86 :: [Word8] -> IO [[AstNodeType]]
liftX86 input = do
    asm <- disasm_buf input
    return (case asm of
      Left _ -> [[(AssertNode "dissasm error")]]
      Right b -> liftAsm b)
