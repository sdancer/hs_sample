module Test1 where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

input = [0xB8, 0x01, 0x00, 0x00, 0x00, 0xB8, 0x02, 0x00, 0x00, 0x00]
first_pass = FirstPass { buffer = input }
simplification = Simplify { buffer = first_pass }

main = hspec $ do
discribe "Test1" $ do
it "returns first_pass result" $ do
first_pass shouldBe ([(setreg 'eax' (lit 1)), (setreg 'eax' (lit 2))])
it "returns simplification result" $ do
simplification shouldBe ("setreg 'eax' (lit 2)")

{- |
asm:
  mov eax, 1
  mov eax, 2

binary: { 0xB8, 0x01, 0x00, 0x00, 0x00, 0xB8, 0x02, 0x00, 0x00, 0x00 }

first pass(individual instruction lift):
(setreg 'eax' (lit 1))
(setreg 'eax' (lit 2))

after simplification:
(setreg 'eax' (lit 2))
-}

-- input = { 0xB8, 0x01, 0x00, 0x00, 0x00, 0xB8, 0x02, 0x00, 0x00, 0x00 }
--
-- first_pass = [
--   (setreg 'eax' (lit 1)),
--   (setreg 'eax' (lit 2))
-- ]

-- output = (setreg 'eax' (lit 2))

{- |
asm:
  mov eax, 1
  add eax, 5
  sub eax, 3

binary: { 0xB8, 0x01, 0x00, 0x00, 0x00, 0x83, 0xC0, 0x05, 0x83, 0xE8, 0x03 }

first pass(individual instruction lift):
  ...

after simplification:
  (setreg 'eax' (lit 2))
-}

-- input = { 0xB8, 0x01, 0x00, 0x00, 0x00, 0x83, 0xC0, 0x05, 0x83, 0xE8, 0x03 }

-- output = (setreg 'eax' (lit 3))

-- context = eval(output)
-- register context eax == 3
