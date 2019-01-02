module Main
where

import           Data.Binary.Get
import qualified Data.ByteString.Lazy       as BS
import           Data.Word
import           Data.Either
import           Hapstone.Capstone
import           Hapstone.Internal.Capstone as Capstone
import           Numeric                    (showHex)
import           System.IO

data Registers = Registers {
  reg_eax :: Int,
  reg_ebx :: Int,
  reg_ecx :: Int,
  reg_edx :: Int,
  reg_esi :: Int,
  reg_edi :: Int,
  reg_ebp :: Int,
  reg_esp :: Int
} deriving (Show)

data State = State {
  blocks :: [Int],
  regs :: Registers
} deriving (Show)


printWord w = do
  System.IO.putStrLn $ showHex(w) ""

loadFile = do
  System.IO.putStrLn "Loading file ..."
  contents <- BS.readFile "blackcipher.aes"
  -- let cutted = BS.drop 0x19BF71A contents
  -- let cutted2 = BS.take 64 cutted
  -- System.IO.putStrLn length(cu) " bytes"
  -- let xxx = BS.foldl toWord8 [] cutted2
  -- let xxx = BS.unpack cutted2
  return $ BS.unpack contents

myAction :: Capstone.Csh -> Capstone.CsInsn -> IO ()
myAction handle insn = System.IO.putStrLn ("0x" ++ a ++ ":\t" ++ m ++ "\t" ++ o)
    where m = mnemonic insn
          o = opStr insn
          a = (showHex $ address insn) ""

myDisasm = do
  intel_asm_buf <- loadFile
  -- mapM printWord intel_asm_buf
  System.IO.putStrLn "Disassembling ..."

  return Disassembler {
    arch = Capstone.CsArchX86 -- ^ Options: CsArchArm, CsArchArm64, CsArchMips, CsArchX86, CsArchPpc, CsArchSparc, CsArchSysz, CsArchXcore
    , modes = [Capstone.CsMode32] -- ^ Modes (some may be combined by adding to the list): CsModeLittleEndian, CsModeArm, CsMode16 (16-bit x86), CsMode32 (32-bit x86), CsMode64 (64-bit x86-64/amd64 or PPC), CsModeThumb, CsModeMclass, CsModeV8 (ARMv8 A32), CsModeMicro, CsModeMips3, CsModeMips32r6, CsModeMipsGp64, CsModeV9 (SparcV9 mode), CsModeBigEndian, CsModeMips32, CsModeMips64
    , buffer = intel_asm_buf -- ^ buffer to disassemble, as [Word8]
    , addr = 0x1DBF71A -- ^ address of first byte in the buffer, as Word64
    , num = 10 -- ^ number of instructions to disassemble (0 for maximum)
    , Hapstone.Capstone.detail = True -- ^ include detailed information? True/False
    , Hapstone.Capstone.skip = Just (defaultSkipdataStruct) -- ^ setup SKIPDATA options, as Maybe CsSkipdataStruct
    , action = defaultAction
    }

printInst :: CsInsn -> [Char]
printInst insn = ("0x" ++ a ++ ":\t" ++ m ++ "\t" ++ o)
  where m = mnemonic insn
        o = opStr insn
        a = (showHex $ address insn) ""

test_foldl :: [[Char]] -> CsInsn -> [[Char]]
test_foldl x insn = printInst insn : x

run_it :: Either CsErr [CsInsn] -> [[Char]]
run_it l = case l of
  Left err -> []
  Right l2 -> foldl test_foldl [] l2

-- disasmIO has signature Disassembler a -> IO (Either CsErr [(CsInsn, a)])
main = do
  d <- myDisasm
  l <- disasmSimpleIO d
  -- let l2 = Right $ Right l
  let x = run_it l
  mapM_ System.IO.putStrLn x
