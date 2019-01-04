module Main
where

import           Data.Binary.Get
import qualified Data.ByteString            as BS
import           Data.Either
import qualified Data.List                  as List
import           Data.Map.Strict
import           Data.Word
import           Debug.Trace                (trace, traceShow)
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
type ProccessedInsnList = [(Word64, ProccessedInsn)]

data State = State {
  blocks :: Map Word64 (Maybe ProccessedInsnList),
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

is_unproccessed_block :: (Word64, Maybe ProccessedInsnList) -> Bool
is_unproccessed_block (_, Nothing) = True
is_unproccessed_block (_, _)       = False

lift_next_block :: State -> BS.ByteString -> IO State
lift_next_block state contents = do
  print $ assocs $ blocks state
  case List.find is_unproccessed_block $ assocs $ blocks state of
    Nothing    -> return state
    Just block -> lift_block state contents block

lift_block :: State -> BS.ByteString -> (Word64, Maybe ProccessedInsnList) -> IO State
lift_block state _ (_, Just _)                  = return state
lift_block state contents (start_addr, Nothing) = do
  insn_list <- disasm_block contents start_addr
  new_state <- case insn_list of
    Left err        -> lift_next_block state contents
    Right insn_list -> return $ do_lift_block state start_addr insn_list []
  return new_state

disasm_block :: BS.ByteString -> Word64 -> IO (Either CsErr [CsInsn])
disasm_block contents start_addr = disasmSimpleIO $ disasm bin start_addr
  where
    bin = getBinPart contents sa 100
    sa = fromIntegral(start_addr)::Int

do_lift_block :: State -> Word64 -> [CsInsn] -> [Word64] -> State
do_lift_block state _ [] _ = state
do_lift_block state bl_addr (insn:xs) addr_stack = do
  let new_as = bl_addr : addr_stack
  let insn_size = fromIntegral(length $ bytes insn)::Word64
  let next_addr = (address insn) + insn_size
  let new_state = add_block_content state bl_addr $ NormalInsn insn
  do_lift_block new_state bl_addr xs new_as

-- set_block_content :: State -> Int -> [CsInsn] -> State
-- set_block_content state addr insn_list = do
--   let ct = Just $ List.map NormalInsn insn_list
--   let bl = insert addr ct $ blocks state
--   state { blocks = bl}

add_block_content :: State -> Word64 -> ProccessedInsn -> State
add_block_content state addr insn = do
  let i = (addr, insn)
  let bls = blocks state
  case bls ! addr of
    Nothing -> state { blocks = insert addr (Just [i]) bls }
    Just bl -> do
      let ct = i : bl
      let xxx = insert addr (Just ct) bls
      state { blocks = xxx }

main :: IO ()
main = do
  contents <- BS.readFile "blackcipher.aes"
  let state = new_state
  aaa <- lift_next_block state contents
  print aaa
