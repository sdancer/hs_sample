module Main
where

import           Data.Binary.Get
import qualified Data.ByteString            as BS
import           Data.Either
import           Data.Word
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
  regs   :: Registers
} deriving (Show)

getBinPart :: BS.ByteString -> Int -> Int -> [Word8]
getBinPart contents from cnt = BS.unpack $ BS.take cnt $ BS.drop from contents

disasm :: [Word8] -> Word64 -> Disassembler ()
disasm intel_asm_buf start_addr = do
  Disassembler {
    arch = Capstone.CsArchX86 -- ^ Options: CsArchArm, CsArchArm64, CsArchMips, CsArchX86, CsArchPpc, CsArchSparc, CsArchSysz, CsArchXcore
    , modes = [Capstone.CsMode32] -- ^ Modes (some may be combined by adding to the list): CsModeLittleEndian, CsModeArm, CsMode16 (16-bit x86), CsMode32 (32-bit x86), CsMode64 (64-bit x86-64/amd64 or PPC), CsModeThumb, CsModeMclass, CsModeV8 (ARMv8 A32), CsModeMicro, CsModeMips3, CsModeMips32r6, CsModeMipsGp64, CsModeV9 (SparcV9 mode), CsModeBigEndian, CsModeMips32, CsModeMips64
    , buffer = intel_asm_buf -- ^ buffer to disassemble, as [Word8]
    , addr = start_addr -- ^ address of first byte in the buffer, as Word64
    , num = 0 -- ^ number of instructions to disassemble (0 for maximum)
    , Hapstone.Capstone.detail = True -- ^ include detailed information? True/False
    , Hapstone.Capstone.skip = Just (defaultSkipdataStruct) -- ^ setup SKIPDATA options, as Maybe CsSkipdataStruct
    , action = defaultAction
    }

liftblock :: State -> State
liftblock state = state

main :: IO ()
main = do
  contents <- BS.readFile "blackcipher.aes"
  let start_addr = 0x1DBF71A
  let max_bytes = 64
  let bin = getBinPart contents start_addr max_bytes
  let x = disasm bin (fromIntegral(start_addr) :: Word64)
  putStrLn "Hahaha"
