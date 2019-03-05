module Test1 where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Lifter
--import Simplify

input = [0xB8, 0x01, 0x00, 0x00, 0x00, 0xB8, 0x02, 0x00, 0x00, 0x00]
first_pass = liftFromBuffer { buffer = input }
simplification = Simplify { buffer = first_pass }

main = hspec $ do
discribe "Test1" $ do
it "returns first_pass result" $ do
first_pass shouldBe ([[SetReg (X86Reg X86RegEax) (BvNode 1 32)],[SetReg (X86Reg X86RegEax) (BvNode 2 32)]])
it "returns simplification result" $ do
simplification shouldBe (setreg 'eax' (lit 2))
