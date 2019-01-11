{-
Dear future me, hope u can understand what i did bellow.
Life is hard, don't blame me.
-}
module Main
where

import           Asm
import           Symbolic
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
import           Util

lift_next_block :: State -> IO State
lift_next_block state = case blocks_queue state of
    x:xs    -> do
      nstate <- trace (">>>> lift next block " ++ showHex x "") $ start_lift_block state x [] 100000
      lift_next_block $ nstate { blocks_queue=xs }
    otherwise -> return state
  where
    contents = mem_data state

start_lift_block :: State -> BlockAddr -> [InsnAddr] -> Int -> IO State
start_lift_block state bl_addr addr_stack cnt = trace (">>> start lift block 0x" ++ showHex bl_addr " " ++ show(keys $ blocks state)) $ if is_proccessed_block bl_addr state -- no infinity loop
  then trace ("--| already proccessed block 0x" ++ showHex bl_addr "") $ return state
  else lift_block state bl_addr bl_addr addr_stack 0 cnt

is_proccessed_block :: BlockAddr -> State -> Bool
is_proccessed_block bl_addr state = List.elem bl_addr $ keys $ blocks state

is_proccessed_insn :: InsnAddr -> [InsnAddr] -> Bool
is_proccessed_insn insn_addr addr_stack = List.elem insn_addr addr_stack

lift_block :: State -> BlockAddr -> InsnAddr -> [InsnAddr] -> Int -> Int -> IO State
lift_block state bl_addr start_addr addr_stack cnt cnt_max = if cnt == cnt_max || is_proccessed_insn start_addr addr_stack
  then trace ("-- already proccessed insn 0x" ++ showHex start_addr " or max " ++ show cnt ++ " " ++ show cnt_max) $ return state
  else do
    insn_list <- disasm_block contents start_addr
    new_state <- case insn_list of
      Right (insn:_) -> do
        result <- proccess_insn state bl_addr insn addr_stack
        case result of
          (new_state, Nothing, _) -> return new_state
          (new_state, Just naddr, new_as) -> lift_block new_state bl_addr naddr new_as (cnt + 1) cnt_max -- trace ("0x" ++ (showHex naddr "")) $
      otherwise -> trace ("---------- end block 0x" ++ showHex bl_addr "") $ return state
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

proccess_insn :: State -> BlockAddr -> CsInsn -> [InsnAddr] -> IO (State, Maybe InsnAddr, [InsnAddr])
proccess_insn state bl_addr insn addr_stack = trace ("--- ESP = " ++ show(fetch_reg_contents X86RegEsp state)) $ if List.notElem (address insn) addr_stack then
  do
    let new_as = (address insn) : addr_stack

    let pi1 = if contains_group X86GrpCall insn then check_grp_call insn else Insn insn
    let pi2 = if contains_group X86GrpRet insn then Skip insn else pi1
    (state3, pi3, naddr) <- if contains_group X86GrpJump insn then check_grp_jump state pi2 block new_as else return (state, pi1, next_addr insn)
    let new_state = add_block_content bl_addr pi3 state3
    case pi3 of
      Break _ -> return (new_state, Nothing, new_as)
      otherwise -> do
        let (_, new_state2) = vproc pi3 new_state
        -- let (_, new_state2) = trace ("#| proc 0x" ++ showHex bl_addr " -> 0x" ++ showHex (address insn) "") $ vproc pi3 new_state
        return (new_state2, Just naddr, new_as)
  else return (state, Just $ next_addr insn, addr_stack)
  where
    block =  findWithDefault [] bl_addr $ blocks state

-- remove junk call
check_grp_call :: CsInsn -> ProccessedInsn
check_grp_call insn = case insn_opr 0 insn of
  Nothing -> Insn insn
  Just op0 -> case value op0 of
      X86.Imm x -> if x == next_addr insn
          then Skip insn
          else Insn insn
      otherwise -> Insn insn

-- check a jump instruction for split block?
check_grp_jump :: State -> ProccessedInsn -> AsmBlock -> [InsnAddr] -> IO (State, ProccessedInsn, InsnAddr)
check_grp_jump state (Skip x) _ _ = return (state, Skip x, next_addr x)
check_grp_jump state (Break x) _ _ = return (state, Break x, next_addr x)
check_grp_jump state (Insn insn) cur_block addr_stack = if mnemonic insn == "jmp"
    then case get_first_opr_value insn of
      X86.Imm x -> trace ("# skip " ++ (insn_to_str insn)) $ if List.elem x addr_stack -- check if this is a loop
        then case List.findIndex (\(a, _) -> a == x) cur_block of
          Nothing -> return (state, Insn insn, next_addr insn)
          Just at -> do
          let (trim, next_bl_addr) = split_block at cur_block $ blocks state
          let new_state = state { blocks = trim }
          return (queue_block next_bl_addr state, Insn insn, next_addr insn)
        else return (state, Skip insn, x)
      otherwise -> return (state, Insn insn, next_addr insn)
    else case get_first_opr_value insn of -- other jump
      X86.Imm jump_addr -> if jump_addr == next_addr insn
        then return (state, Skip insn, next_addr insn)
        else trace ((insn_to_str insn) ++ "# fork branch 0x" ++ (showHex jump_addr "")) $ do
          let right_addr = (next_addr insn)
          lstate <- trace ("- lift subblock 0x" ++ showHex jump_addr " " ++ (show $ length addr_stack)) $ start_lift_block state jump_addr addr_stack 10
          rstate <- trace ("- lift subblock 2 0x" ++ showHex right_addr "" ++ (show $ keys $ blocks state)) $ start_lift_block state right_addr addr_stack 10
          let (la, _):_ = List.filter skip_junk $ blocks lstate ! jump_addr
          let (ra, _):_ = List.filter skip_junk $ blocks rstate ! right_addr
          if la == ra
            then return (state, Skip insn, next_addr insn)
            else if (List.elem jump_addr $ keys $ blocks state) || (List.elem jump_addr $ blocks_queue rstate)
              then return (state, Insn insn, next_addr insn) -- ??? fix infinity loop, right?
              else return (queue_block jump_addr state, Insn insn, next_addr insn)
      otherwise -> return (state, Break insn, next_addr insn)

-- for skipping junk instructions
skip_junk :: (InsnAddr, ProccessedInsn) -> Bool
skip_junk (_, Insn _) = True
skip_junk _           = False

block_address :: AsmBlock -> BlockAddr
block_address [] = 0
block_address ((addr, _):_) = addr

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

main :: IO ()
main = do
  contents <- BS.readFile "blackcipher.aes"
  state <- lift_next_block $ new_state contents
  compile_blocks $ toAscList $ blocks state
  print $ keys $ blocks state
  print $ blocks_queue state
