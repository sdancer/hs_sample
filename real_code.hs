module Main
where

import           Data.Binary.Get
import qualified Data.ByteString            as BS
import           Data.Either
import qualified Data.List                  as List
import           Data.Map.Strict
import           Data.Word
import           Hapstone.Capstone
import           Hapstone.Internal.Capstone as Capstone
import           Numeric                    (showHex)
import           System.IO

data Registers = Registers {
  eax :: Int,
  ebx :: Int,
  ecx :: Int,
  edx :: Int,
  esi :: Int,
  edi :: Int,
  ebp :: Int,
  esp :: Int
} deriving (Show)

data ProccessedInsn = NormalInsn CsInsn | JunkInsn String deriving (Show)
type ProccessedInsnList = [ProccessedInsn]

data State = State {
  blocks :: Map Int (Maybe ProccessedInsnList),
  regs   :: Registers
} deriving (Show)

getBinPart :: BS.ByteString -> Int -> Int -> [Word8]
getBinPart contents from cnt = BS.unpack $ BS.take cnt $ BS.drop from contents

disasm :: [Word8] -> Word64 -> Disassembler ()
disasm intel_asm_buf start_addr = Disassembler {
    arch = Capstone.CsArchX86 -- ^ Options: CsArchArm, CsArchArm64, CsArchMips, CsArchX86, CsArchPpc, CsArchSparc, CsArchSysz, CsArchXcore
    , modes = [Capstone.CsMode32] -- ^ Modes (some may be combined by adding to the list): CsModeLittleEndian, CsModeArm, CsMode16 (16-bit x86), CsMode32 (32-bit x86), CsMode64 (64-bit x86-64/amd64 or PPC), CsModeThumb, CsModeMclass, CsModeV8 (ARMv8 A32), CsModeMicro, CsModeMips3, CsModeMips32r6, CsModeMipsGp64, CsModeV9 (SparcV9 mode), CsModeBigEndian, CsModeMips32, CsModeMips64
    , buffer = intel_asm_buf -- ^ buffer to disassemble, as [Word8]
    , addr = start_addr -- ^ address of first byte in the buffer, as Word64
    , num = 0 -- ^ number of instructions to disassemble (0 for maximum)
    , Hapstone.Capstone.detail = True -- ^ include detailed information? True/False
    , Hapstone.Capstone.skip = Just (defaultSkipdataStruct) -- ^ setup SKIPDATA options, as Maybe CsSkipdataStruct
    , action = defaultAction
    }

new_state :: State
new_state = State {blocks=init_blocks,regs=Registers {eax=0, ebx=0, ecx=0, edx=0, esi=0, edi=0, ebp=0, esp=0}}
  where
    init_blocks = fromList [(0x1DBF71A, Nothing)]

is_unproccessed_block :: (Int, Maybe ProccessedInsnList) -> Bool
is_unproccessed_block (_, Nothing) = True
is_unproccessed_block (_, _)       = False

lift_next_block :: State -> BS.ByteString -> IO State
lift_next_block state contents = do
  print $ assocs $ blocks state
  case List.find is_unproccessed_block $ assocs $ blocks state of
    Nothing    -> return state
    Just block -> lift_block state contents block

lift_block :: State -> BS.ByteString -> (Int, Maybe ProccessedInsnList) -> IO State
lift_block state _ (_, Just _)                  = return state
lift_block state contents (start_addr, Nothing) = do
  insn_list <- disasm_block contents start_addr
  new_state <- case insn_list of
    Left err        -> lift_next_block state contents
    Right insn_list -> return $ set_block_content state start_addr insn_list
  return new_state

disasm_block :: BS.ByteString -> Int -> IO (Either CsErr [CsInsn])
disasm_block contents start_addr = disasmSimpleIO $ disasm bin (fromIntegral(start_addr) :: Word64)
  where
    bin = getBinPart contents start_addr 100

-- normal_insn i = NormalInsn i

set_block_content :: State -> Int -> [CsInsn] -> State
set_block_content state addr insn_list = do
  let ct = Just $ List.map NormalInsn insn_list
  let bl = insert addr ct $ blocks state
  state { blocks = bl}

main :: IO ()
main = do
  contents <- BS.readFile "blackcipher.aes"
  let state = new_state
  aaa <- lift_next_block state contents
  print aaa
