{-
Dear future me, hope u can understand what i did bellow.
Life is hard, don't blame me.
-}
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
import           Hapstone.Internal.X86      as X86
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

data AbstractOp = AbstractOp {
  optype :: [Char],
  op1    :: X86OpType,
  op2    :: X86OpType
} deriving (Show)

data StackValue = IntVal Int | StrVal [Char]  | AbsVal AbstractOp deriving (Show)

data State = State {
  blocks :: Map Word64 (Maybe ProccessedInsnList),
  regs   :: Registers,
  stack  :: Map Word32 StackValue
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
new_state = State {blocks=init_blocks,regs=Registers {eax=0, ebx=0, ecx=0, edx=0, esi=0, edi=0, ebp=0, esp=0}, stack=fromList([])}
  where
    init_blocks = fromList [(0x1DBF71A, Nothing)]

is_unproccessed_block :: (Word64, Maybe ProccessedInsnList) -> Bool
is_unproccessed_block (_, Nothing) = True
is_unproccessed_block (_, _)       = False

lift_next_block :: State -> BS.ByteString -> IO State
lift_next_block state contents = case List.find is_unproccessed_block $ assocs $ blocks state of
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
    bin = getBinPart contents addr 100
    addr = sa - ba
    sa = fromIntegral(start_addr)::Int
    ba = 0x400000

do_lift_block :: State -> Word64 -> [CsInsn] -> [Word64] -> State
do_lift_block state bl_addr [] _ = state { blocks = reversed } -- reverse proccessed instruction list
  where
    reversed = insert bl_addr reversed_bl bls
    reversed_bl = case pinsns of
      Nothing   -> Nothing
      Just list -> Just $ List.reverse list
    pinsns = bls ! bl_addr
    bls = blocks state
do_lift_block state bl_addr (insn:xs) addr_stack = if List.notElem (address insn) addr_stack then
  do
    let new_as = bl_addr : addr_stack

    let pi1 = if contains_group X86GrpCall insn then check_grp_call insn else NormalInsn insn
    let pi2 = if contains_group X86GrpRet insn then JunkInsn "skip" else pi1
    let (state3, pi3) = if contains_group X86GrpJump insn then check_grp_jump state pi2 block addr_stack else (state, pi1)

    let new_state = add_block_content state3 bl_addr pi3
    do_lift_block new_state bl_addr xs new_as
  else state
  where
    block = case (blocks state) ! bl_addr of
      Nothing -> []
      Just l -> l

check_grp_call :: CsInsn -> ProccessedInsn
check_grp_call insn = case get_first_opr insn of
  Nothing -> NormalInsn insn
  Just op0 -> case value op0 of
      X86.Imm x -> if x == next_addr insn
          then JunkInsn "skip"
          else NormalInsn insn
      otherwise -> NormalInsn insn

-- check a jump instruction for split block?
check_grp_jump :: State -> ProccessedInsn -> ProccessedInsnList -> [Word64] -> (State, ProccessedInsn)
check_grp_jump state (JunkInsn x) _ _ = (state, JunkInsn x)
check_grp_jump state (NormalInsn insn) block addr_stack = if mnemonic insn == "jmp"
    then case get_first_opr_value insn of
      Just (X86.Imm x) -> if List.elem x addr_stack -- check if this is a loop
        then case List.find (\(a, _) -> a == x) block of
          Nothing -> (state, NormalInsn insn)
          Just (a, _) -> (state { blocks = split_block a block $ blocks state}, NormalInsn insn)
        else (state, JunkInsn "skip")
      otherwise -> trace ((show $ address insn) ++ "jmp to unknown location") $ (state, NormalInsn insn)
    else (state, NormalInsn insn)

-- return first operand in a instruction
get_first_opr :: CsInsn -> Maybe CsX86Op
get_first_opr insn = case Capstone.detail insn of
  Nothing -> Nothing
  Just d -> case archInfo d of
    Nothing        -> Nothing
    Just (X86 ari) -> case operands ari of
      op0:_     -> Just op0
      otherwise -> Nothing

get_first_opr_value :: CsInsn -> Maybe CsX86OpValue
get_first_opr_value insn = case get_first_opr insn of
  Nothing -> Nothing
  Just op -> Just $ value op

next_addr :: CsInsn -> Word64
next_addr insn = (address insn) + insn_size
  where
    insn_size = fromIntegral(length $ bytes insn)::Word64

contains_group :: X86InsnGroup -> CsInsn -> Bool
contains_group gr insn = case Capstone.detail insn of
  Nothing -> False
  Just d  -> do
    let grw8 = fromIntegral(fromEnum gr) :: Word8
    List.elem grw8 $ groups d

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

split_block :: Word64 -> ProccessedInsnList -> Map Word64 (Maybe ProccessedInsnList) -> Map Word64 (Maybe ProccessedInsnList)
split_block _ _ blocks = blocks -- fixme: do split

compile_blocks :: [(Word64, (Maybe ProccessedInsnList))] -> IO ()
compile_blocks [] = return ()
compile_blocks ((_, Nothing):xs) = compile_blocks xs
compile_blocks ((_, Just ilist):xs) = do
    compile_block ilist
    compile_blocks xs

compile_block :: ProccessedInsnList -> IO ()
compile_block [] = return ()
compile_block ((_, JunkInsn _) : xs) = compile_block xs
compile_block ((_, NormalInsn insn) : xs) = do
  putStrLn $ insn_to_str insn
  compile_block xs

-- Convert a instruction to string
insn_to_str :: CsInsn -> [Char]
insn_to_str insn = "0x" ++ a ++ ":\t" ++ m ++ "\t" ++ o
    where m = mnemonic insn
          o = opStr insn
          a = (showHex $ address insn) ""

main :: IO ()
main = do
  contents <- BS.readFile "blackcipher.aes"
  state <- lift_next_block new_state contents
  compile_blocks $ toAscList $ blocks state
  -- print nstate
