{-
Dear future me, hope u can understand what i did bellow.
Life is hard, don't blame me.
-}
module Main
where

import           Asm
import           Util
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

lift_next_block :: State -> IO State
lift_next_block state = case blocks_queue state of
    x:xs    -> do
      nstate <- lift_block state x x []
      return nstate { blocks_queue=xs }
    otherwise -> return state
  where
    contents = mem_data state

lift_block :: State -> BlockAddr -> InsnAddr -> [InsnAddr] -> IO State
lift_block state bl_addr start_addr addr_stack = do
  insn_list <- disasm_block contents start_addr
  new_state <- case insn_list of
    Right (insn:_) -> case proccess_insn state bl_addr insn addr_stack of
      (new_state, Nothing, _) -> return new_state
      (new_state, Just naddr, new_as) -> trace ("0x" ++ (showHex naddr "")) $ lift_block new_state bl_addr naddr new_as
    otherwise -> return state
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

proccess_insn :: State -> BlockAddr -> CsInsn -> [InsnAddr] -> (State, Maybe InsnAddr, [InsnAddr])
-- proccess_insn state bl_addr [] _ = state { blocks = reversed } -- reverse proccessed instruction list
--   where
--     reversed = insert bl_addr reversed_bl bls
--     reversed_bl = List.reverse $ bls ! bl_addr
--     bls = blocks state
proccess_insn state bl_addr insn addr_stack = if List.notElem (address insn) addr_stack then
  do
    let new_as = (address insn) : addr_stack

    let pi1 = if contains_group X86GrpCall insn then check_grp_call insn else Insn insn
    let pi2 = if contains_group X86GrpRet insn then Skip insn else pi1
    let (state3, pi3, naddr) = if contains_group X86GrpJump insn then check_grp_jump state pi2 block new_as else (state, pi1, next_addr insn)
    let new_state = add_block_content state3 bl_addr pi3
    case pi3 of
      Break _ -> (new_state, Nothing, new_as)
      otherwise -> do
        let new_state2 = vproc new_state insn
        (new_state2, Just naddr, new_as)
  else (state, Just $ next_addr insn, addr_stack)
  where
    block = (blocks state) ! bl_addr

-- remove junk call
check_grp_call :: CsInsn -> ProccessedInsn
check_grp_call insn = case get_first_opr insn of
  Nothing -> Insn insn
  Just op0 -> case value op0 of
      X86.Imm x -> if x == next_addr insn
          then Skip insn
          else Insn insn
      otherwise -> Insn insn

-- check a jump instruction for split block?
check_grp_jump :: State -> ProccessedInsn -> AsmBlock -> [InsnAddr] -> (State, ProccessedInsn, InsnAddr)
check_grp_jump state (Skip x) _ _ = (state, Skip x, next_addr x)
check_grp_jump state (Break x) _ _ = (state, Break x, next_addr x)
check_grp_jump state (Insn insn) cur_block addr_stack = if mnemonic insn == "jmp"
    then case get_first_opr_value insn of
      Just (X86.Imm x) -> trace (";jump to 0x" ++ (showHex x "")) $ if List.elem x addr_stack -- check if this is a loop
        then case List.findIndex (\(a, _) -> a == x) cur_block of
          Nothing -> (state, Insn insn, address insn)
          Just at -> (state { blocks = split_block at cur_block $ blocks state}, Insn insn, next_addr insn)
        else (state, Skip insn, x)
      otherwise -> trace ((show $ address insn) ++ "jmp to unknown location") $ (state, Insn insn, next_addr insn)
    else case get_first_opr_value insn of
      Just (X86.Imm jump_addr) -> if (next_addr insn) == next_addr insn
        then (state, Skip insn, next_addr insn)
        else trace ";fork branch" $ do
          let right_addr = (next_addr insn)
          let lstate = lift_block state jump_addr jump_addr addr_stack
          let rstate = lift_block state right_addr right_addr addr_stack
          let (la, _):_ = List.filter skip_junk $ blocks state ! jump_addr
          let (ra, _):_ = List.filter skip_junk $ blocks state ! right_addr
          if la == ra
            then (state, Skip insn, next_addr insn)
            else if List.elem jump_addr $ keys $ blocks state
              then (state, Insn insn, next_addr insn)
              else (state { blocks_queue=jump_addr:(blocks_queue state)}, Insn insn, next_addr insn)
      otherwise -> (state, Break insn, next_addr insn)

-- for skipping junk instructions
skip_junk :: (InsnAddr, ProccessedInsn) -> Bool
skip_junk (_, Insn _) = True
skip_junk _                 = False

-- check if an instruction has a group (call/ret/jump/...)
contains_group :: X86InsnGroup -> CsInsn -> Bool
contains_group gr insn = case Capstone.detail insn of
  Nothing -> False
  Just d  -> do
    let grw8 = fromIntegral(fromEnum gr) :: Word8
    List.elem grw8 $ groups d

-- add an procces instruction to block
add_block_content :: State -> InsnAddr -> ProccessedInsn -> State
add_block_content state addr insn = do
  let i = (addr, insn)
  let bls = blocks state
  let bl = findWithDefault [] addr bls
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
compile_blocks ((_, ilist):xs) = do
    compile_block ilist
    compile_blocks xs

-- for now, just print block's instructions
compile_block :: AsmBlock -> IO ()
compile_block [] = return ()
compile_block ((addr, Skip insn) : xs) = do
  putStrLn $ ";skip\t -->\t" ++ insn_to_str insn
  compile_block xs
compile_block ((addr, Break insn) : xs) = do
  putStrLn $ ";break\t -->\t" ++ insn_to_str insn
  compile_block xs
compile_block ((_, Insn insn) : xs) = do
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
  print $ keys $ blocks state
  print $ blocks_queue state
