module Main where

import Test.Tasty
import Test.Tasty.HUnit
--import Test.Tasty.SmallCheck as SC
--import Test.HUnit.Base

--import qualified Data.ByteString            as BS

import Lifter
import Ast

import Data.Word
--import Examples

--first_pass = liftFromBuffer { buffer = input }
--simplification = Simplify { buffer = first_pass }

-- main = hspec $ do
-- discribe "Test1" $ do
-- it "returns first_pass result" $ do
-- first_pass shouldBe ([[SetReg (X86Reg X86RegEax) (BvNode 1 32)],[SetReg (X86Reg X86RegEax) (BvNode 2 32)]])
-- it "returns simplification result" $ do
-- simplification shouldBe (setreg 'eax' (lit 2))


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
    "foo" @?= "bar"

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
