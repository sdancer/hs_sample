module Test1 where

  import Data.Word
  import Numeric (showHex)
  
  import Hapstone.Capstone
  import Hapstone.Internal.Capstone as Capstone
  
  import Test.Hspec
  import Test.QuickCheck
  
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
  
  -- output = (setreg 'eax' (lit 2))
  
  -- use example from Capstone: http://www.capstone-engine.org/lang_python.html
  input = [0xB8, 0x01, 0x00, 0x00, 0x00, 0xB8, 0x02, 0x00, 0x00, 0x00] :: [Word8]
  
  myAction :: Capstone.Csh -> Capstone.CsInsn -> IO ()
  myAction handle insn = putStrLn ("0x" ++ a ++ ":\t" ++ m ++ "\t" ++ o)
      where m = mnemonic insn
            o = opStr insn
            a = (showHex $ address insn) ""
  
  myDisasm = Disassembler { 
      arch = Capstone.CsArchX86 -- ^ Options: CsArchArm, CsArchArm64, CsArchMips, CsArchX86, CsArchPpc, CsArchSparc, CsArchSysz, CsArchXcore
      , modes = [Capstone.CsMode64] -- ^ Modes (some may be combined by adding to the list): CsModeLittleEndian, CsModeArm, CsMode16 (16-bit x86), CsMode32 (32-bit x86), CsMode64 (64-bit x86-64/amd64 or PPC), CsModeThumb, CsModeMclass, CsModeV8 (ARMv8 A32), CsModeMicro, CsModeMips3, CsModeMips32r6, CsModeMipsGp64, CsModeV9 (SparcV9 mode), CsModeBigEndian, CsModeMips32, CsModeMips64
      , buffer = intel_asm_buf -- ^ buffer to disassemble, as [Word8]
      , addr = 0x1000 -- ^ address of first byte in the buffer, as Word64
      , num = 0 -- ^ number of instructions to disassemble (0 for maximum)
      , Hapstone.Capstone.detail = True -- ^ include detailed information? True/False
      , skip = Just (defaultSkipdataStruct) -- ^ setup SKIPDATA options, as Maybe CsSkipdataStruct
      , action = myAction -- ^ action to run on each instruction, a function with signature Csh -> CsInsn -> IO a; default is defaultAction
      }
  
  first_pass = FirstPass { buffer = myDisasm }
  simplification = Simplify { buffer = first_pass }
  
  main = hspec $ do
    discribe "Test1" $ do
        it "returns first_pass result" $ do
            first_pass `shouldBe` ([(SetReg 'eax' (DecimalNode 1)), (SetReg 'eax' (DecimalNode 2))])
        it "returns simplification result" $ do
            simplification `shouldBe` (SetReg 'eax' (DecimalNode 2))
  
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
  
  -- output = (setreg 'eax' (lit 3))