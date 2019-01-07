{-
Dear future me, hope u can understand what i did bellow.
Life is hard, don't blame me.
-}
module Main
where

import           Asm
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

new_state :: BS.ByteString -> State
new_state contents = State {mem_data=contents, blocks=empty, blocks_queue=[0x1DBF71A],regs=Registers {eax=0, ebx=0, ecx=0, edx=0, esi=0, edi=0, ebp=0, esp=0}, stack=fromList([])}

lift_next_block :: State -> IO State
lift_next_block state = case blocks_queue state of
    x:xs    -> do
      nstate <- lift_block state x []
      return nstate { blocks_queue=xs }
    otherwise -> return state
  where
    contents = mem_data state

lift_block :: State -> BlockAddr -> [InsnAddr] -> IO State
lift_block state start_addr addr_stack = do
  insn_list <- disasm_block contents start_addr
  new_state <- case insn_list of
    Left err        -> return state
    Right insn_list -> return $ do_lift_block state start_addr insn_list addr_stack
  return new_state
  where
    contents = mem_data state

disasm_block :: BS.ByteString -> BlockAddr -> IO (Either CsErr [CsInsn])
disasm_block contents start_addr = disasmSimpleIO $ disasm bin start_addr
  where
    bin = getBinPart contents addr 100
    addr = sa - ba
    sa = fromIntegral(start_addr)::Int
    ba = 0x400000

do_lift_block :: State -> BlockAddr -> [CsInsn] -> [InsnAddr] -> State
do_lift_block state bl_addr [] _ = state { blocks = reversed } -- reverse proccessed instruction list
  where
    reversed = insert bl_addr reversed_bl bls
    reversed_bl = List.reverse $ bls ! bl_addr
    bls = blocks state
do_lift_block state bl_addr (insn:xs) addr_stack = if List.notElem (address insn) addr_stack then
  do
    let new_as = bl_addr : addr_stack

    let pi1 = if contains_group X86GrpCall insn then check_grp_call insn else NormalInsn insn
    let pi2 = if contains_group X86GrpRet insn then JunkInsn "skip" else pi1
    let (state3, pi3) = if contains_group X86GrpJump insn then check_grp_jump state pi2 block addr_stack else (state, pi1)
    let new_state = add_block_content state3 bl_addr pi3
    case pi3 of
      JunkInsn "skip" -> new_state
      otherwise -> do
        let new_state2 = vproc new_state insn
        do_lift_block new_state2 bl_addr xs new_as
  else state
  where
    block = (blocks state) ! bl_addr

check_grp_call :: CsInsn -> ProccessedInsn
check_grp_call insn = case get_first_opr insn of
  Nothing -> NormalInsn insn
  Just op0 -> case value op0 of
      X86.Imm x -> if x == next_addr insn
          then JunkInsn "skip"
          else NormalInsn insn
      otherwise -> NormalInsn insn

-- check a jump instruction for split block?
check_grp_jump :: State -> ProccessedInsn -> AsmBlock -> [InsnAddr] -> (State, ProccessedInsn)
check_grp_jump state (JunkInsn x) _ _ = (state, JunkInsn x)
check_grp_jump state (NormalInsn insn) cur_block addr_stack = if mnemonic insn == "jmp"
    then case get_first_opr_value insn of
      Just (X86.Imm x) -> if List.elem x addr_stack -- check if this is a loop
        then case List.findIndex (\(a, _) -> a == x) cur_block of
          Nothing -> (state, NormalInsn insn)
          Just at -> (state { blocks = split_block at cur_block $ blocks state}, NormalInsn insn)
        else (state, JunkInsn "skip")
      otherwise -> trace ((show $ address insn) ++ "jmp to unknown location") $ (state, NormalInsn insn)
    else case get_first_opr_value insn of
      Just (X86.Imm jump_addr) -> if (next_addr insn) == next_addr insn
        then (state, JunkInsn "skip")
        else trace "fork branch" $ do
          let right_addr = (next_addr insn)
          let lstate = lift_block state jump_addr addr_stack
          let rstate = lift_block state right_addr addr_stack
          let (la, _):_ = List.filter skip_junk $ blocks state ! jump_addr
          let (ra, _):_ = List.filter skip_junk $ blocks state ! right_addr
          if la == ra
            then (state, JunkInsn "skip")
            else if List.elem jump_addr $ keys $ blocks state
              then (state, NormalInsn insn)
              else (state { blocks_queue=jump_addr:(blocks_queue state)}, NormalInsn insn)
      otherwise -> (state, JunkInsn "break")

skip_junk :: (InsnAddr, ProccessedInsn) -> Bool
skip_junk (_, NormalInsn _) = True
skip_junk _                 = False

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

next_addr :: CsInsn -> InsnAddr
next_addr insn = (address insn) + insn_size
  where
    insn_size = fromIntegral(length $ bytes insn)::InsnAddr

contains_group :: X86InsnGroup -> CsInsn -> Bool
contains_group gr insn = case Capstone.detail insn of
  Nothing -> False
  Just d  -> do
    let grw8 = fromIntegral(fromEnum gr) :: Word8
    List.elem grw8 $ groups d

add_block_content :: State -> InsnAddr -> ProccessedInsn -> State
add_block_content state addr insn = do
  let i = (addr, insn)
  let bls = blocks state
  let bl = bls ! addr
  let ct = i : bl
  let xxx = insert addr ct bls
  state { blocks = xxx }

-- split current block at a position into multiple blocks
split_block :: Int -> AsmBlock -> AsmBlocks -> AsmBlocks
split_block at block blocks = do
  let (old_bl, new_bl) = List.splitAt at block
  let ((oaddr, _):_) = old_bl
  let ((naddr, _):_) = new_bl
  let new_blocks = insert oaddr old_bl $ blocks
  insert naddr new_bl $ new_blocks

compile_blocks :: [(Word64, AsmBlock)] -> IO ()
compile_blocks [] = return ()
-- compile_blocks ((_, Nothing):xs) = compile_blocks xs
compile_blocks ((_, ilist):xs) = do
    compile_block ilist
    compile_blocks xs

compile_block :: AsmBlock -> IO ()
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
  state <- lift_next_block $ new_state contents
  compile_blocks $ toAscList $ blocks state
  -- print nstate
