module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Lifter
import Ast
import Hapstone.Internal.Capstone as Capstone
import Phasses
import EvalAst
import Hapstone.Internal.X86 as X86

main :: IO ()
main = defaultMain $
  testGroup "Lifter" [
      testLift
    , testSym
    , deadSetRegTest0
    , deadSetRegTest1
    --, testBlockOne
  ]

testLift :: TestTree

testLift =
  testCase "simple lift buf" $ do
    let modes = [Capstone.CsMode32]
    let input = [0xB8, 0x01, 0x00, 0x00, 0x00, -- mov eax, 0x1
                 0xB8, 0x02, 0x00, 0x00, 0x00] -- mov eax, 0x2
    l <- decompile modes (allMemory modes) input
    l @?= Compound (-1) [Compound (-1) [Compound 0 [Compound 1 [], Compound 4
            [SetReg 5 (1472,1504) (BvExpr (10,32)),
            SetReg 6 (0,32) (BvExpr (2,32))]]]]

testSym :: TestTree

testSym =
  testCase "symbolic" $ do
    let modes = [Capstone.CsMode32]
    let input = [0xB8, 0x00, 0x00, 0x00, 0x00, -- mov eax, 0x0
                0x83, 0xC0, 0x0A, -- add eax, 0xa
                0x83, 0xC0, 0x0F, -- add eax, 0xf
                0x83, 0xE8, 0x03, -- sub eax, 0x3
                0x89, 0xD8, -- mov eax, ebx
                0x83, 0xC0, 0x14] -- add eax, 0x14
    l <- decompile modes (allMemory modes) input
    l @?= Compound (-1) [Compound (-1) [Compound 0 [Compound 1 [],Compound 4 [], Compound 13 [],
            Compound 22 [],Compound 31 [SetReg 33 (0,32) (GetReg (64,96))], Compound 34
              [SetReg 35 (1472,1504) (BvExpr (19,32)),
              SetReg 36 (0,32) (BvaddExpr (ReferenceExpr 32 33) (BvExpr (20,32)))]]]]

-- The approach taken in the following tests is to lift executable code into the IR,
-- decompile executable code into the IR, numerically execute both IRs with the same
-- initial machine state, and assert that the final machine states are equal. Various
-- properties of the transformed IR (such as the number of Load expressions it contains)
-- are also checked.

deadSetRegTest0 :: TestTree

deadSetRegTest0 =
  testCase "dead SetReg test 0" $ do
    let modes = [Capstone.CsMode32]
    let input = [0xB8, 0x0A, 0x00, 0x00, 0x00, -- mov eax, 0xa
                0xB8, 0x14, 0x00, 0x00, 0x00] -- mov eax, 0x14
    let initState = zeroInsnPtr (numExecContext modes)
    lifted <- lift modes input
    decompiled <- decompile modes (allMemory modes) input
    -- Ensure that both IRs do the same things to the machine
    fetchExec initState lifted @?= exec initState decompiled

deadSetRegTest1 :: TestTree

deadSetRegTest1 =
  testCase "dead SetReg test 1" $ do
    let modes = [Capstone.CsMode32]
    let input = [0xB8, 0x0A, 0x00, 0x00, 0x00, -- mov eax, 0xa
                0xB8, 0x14, 0x00, 0x00, 0x00] -- mov eax, 0x14
    decompiled <- decompile modes (allMemory modes) input
    let count acc (SetReg _ reg _) | reg == fromX86Reg X86RegEax = acc + 1
        count acc _ = acc
    -- Ensure that EAX is only set once
    foldl count (0 :: Int) (flattenStmt decompiled) @?= 1
    
