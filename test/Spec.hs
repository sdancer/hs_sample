module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Lifter
import Ast
import Hapstone.Internal.Capstone as Capstone
import Phasses
import EvalAst
import Hapstone.Internal.X86 as X86
import BitVector
import Control.Exception

main :: IO ()
main = defaultMain $
  testGroup "Lifter" [
      testLift
    , testSym
    , deadSetRegTest0
    , deadSetRegTest1
    , deadSetRegTest2
    , deadSetRegTest3
    , deadStoreTest0
    , referenceTest0
    --, testBlockOne
  ]

-- A Pattern is a predicate that returns True for values matching the Pattern, and False
-- otherwise. A Pattern is treated as if it returned False in the case that applying it
-- yields a PatternMatchFail.

type Pattern a = a -> Bool

-- Count the number of items in the given list that match the given pattern

countPattern :: [a] -> Pattern a -> IO Int

countPattern [] _ = return 0

countPattern (x:xs) pat = catch
  (do
    a <- evaluate $ pat x
    b <- countPattern xs pat
    return ((fromEnum a) + b))
  (\e -> let _ = (e :: PatternMatchFail) in countPattern xs pat)

-- Count the number of items in the given statement that match the given pattern

countStmtPattern :: Stmt a b c d -> Pattern (Stmt a b c d) -> IO Int

countStmtPattern stmt pat = countPattern (flattenStmt stmt) pat

testLift :: TestTree

testLift =
  testCase "simple lift buf" $ do
    let modes = [Capstone.CsMode32]
    let input = [0xB8, 0x01, 0x00, 0x00, 0x00, -- mov eax, 0x1
                 0xB8, 0x02, 0x00, 0x00, 0x00] -- mov eax, 0x2
    l <- decompile modes (allMemory modes) input
    l @?= Compound (-1) [Compound (-1) [Compound 0 [Compound 1 [], Compound 4
            [SetReg 5 (1472,1504) (BvExpr (Bv(10,32))),
            SetReg 6 (0,32) (BvExpr (Bv(2,32)))]]]]

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
              [SetReg 35 (1472,1504) (BvExpr (Bv(19,32))),
              SetReg 36 (0,32) (BvaddExpr (ReferenceExpr 32 33) (BvExpr (Bv(20,32))))]]]]

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
    -- Ensure that EAX is only set once
    count <- countStmtPattern decompiled (\(SetReg _ reg _) -> reg == fromX86Reg X86RegEax)
    count @?= 1

deadSetRegTest2 :: TestTree

deadSetRegTest2 =
  testCase "dead SetReg test 2" $ do
    let modes = [Capstone.CsMode32]
    let input = [0xB8, 0x0A, 0x00, 0x00, 0x00, -- mov eax, 0xa
                0x6A, 0x1E, -- push 0x1e
                0x8D, 0x40, 0x14] -- lea eax,[eax+0x14]
    let initState = zeroStackReg $ zeroInsnPtr (numExecContext modes)
    lifted <- lift modes input
    decompiled <- decompile modes (allMemory modes) input
    -- Ensure that both IRs do the same things to the machine
    fetchExec initState lifted @?= exec initState decompiled

deadSetRegTest3 :: TestTree

deadSetRegTest3 =
  testCase "dead SetReg test 3" $ do
    let modes = [Capstone.CsMode32]
    let input = [0xB8, 0x0A, 0x00, 0x00, 0x00, -- mov eax, 0xa
                0x6A, 0x1E, -- push 0x1e
                0x8D, 0x40, 0x14] -- lea eax,[eax+0x14]
    decompiled <- decompile modes (allMemory modes) input
    -- Ensure that EAX is only set once
    count <- countStmtPattern decompiled (\(SetReg _ reg _) -> reg == fromX86Reg X86RegEax)
    count @?= 1

deadStoreTest0 :: TestTree

deadStoreTest0 =
  testCase "dead Store test 0" $ do
    let modes = [Capstone.CsMode32]
    let input = [0xC7, 0x00, 0x0A, 0x00, 0x00, 0x00, -- mov DWORD PTR [eax], 0xa
                0x8B, 0x18, -- mov ebx, DWORD PTR [eax]
                0x83, 0xC3, 0x1E, -- add ebx, 0x1e
                0x89, 0x18] -- mov DWORD PTR [eax], ebx
    decompiled <- decompile modes (allMemory modes) input
    -- Ensure that DWORD PTR [EAX] is only set once
    varCount <- countStmtPattern decompiled (\(Store _ (GetReg reg) _) -> reg == fromX86Reg X86RegEax)
    varCount @?= 1
    -- Ensure that 0x28 is stored in DWORD PTR [EAX] is exactly once
    valCount <- countStmtPattern decompiled (\(Store _ (GetReg reg) (BvExpr 0x28)) -> reg == fromX86Reg X86RegEax)
    valCount @?= 1

referenceTest0 :: TestTree

referenceTest0 =
  testCase "reference test 0" $ do
    let modes = [Capstone.CsMode32]
    let input = [0x57, -- push edi
                0x56, -- push esi
                0x5B, -- pop ebx
                0x58] -- pop eax
    decompiled <- decompile modes (allMemory modes) input
    -- Ensure that a reference is stored in EBX is exactly once
    ebxCount <- countStmtPattern decompiled (\(SetReg _ reg (ReferenceExpr 32 _)) -> reg == fromX86Reg X86RegEbx)
    ebxCount @?= 1
    -- Ensure that a reference is stored in EAX is exactly once
    eaxCount <- countStmtPattern decompiled (\(SetReg _ reg (ReferenceExpr 32 _)) -> reg == fromX86Reg X86RegEax)
    eaxCount @?= 1

