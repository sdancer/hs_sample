module Asm where

import           Data.Bits
import qualified Data.ByteString            as BS
import qualified Data.List                  as List
import           Data.Map.Strict
import           Data.Word
import           Hapstone.Capstone
import           Hapstone.Internal.Capstone as Capstone
import           Hapstone.Internal.X86      as X86
import           Numeric                    (showHex)

data Registers = Registers {
  eax    :: AsmValue,
  ebx    :: AsmValue,
  ecx    :: AsmValue,
  edx    :: AsmValue,
  esi    :: AsmValue,
  edi    :: AsmValue,
  ebp    :: AsmValue,
  esp    :: AsmValue,
  eflags :: Word32
} deriving (Show)
data ProccessedInsn = Insn CsInsn | Skip CsInsn | Break CsInsn | WallSE CsInsn deriving (Show)
type InsnAddr = Word64
type BlockAddr = Word64
type AsmBlock = [(InsnAddr, ProccessedInsn)]
type AsmBlocks = Map BlockAddr AsmBlock
-- data AbstractOpVal = SubAbsVal AbstractOp | RealVal CsX86OpValue deriving (Show)
data AbstractOp = AbstractOp {
  optype :: [Char],
  op1    :: AsmValue,
  op2    :: AsmValue
} deriving (Show, Eq)
data AsmValue = NumVal Word64 | StrVal [Char]  | AbsVal AbstractOp | Flags Word32 | InitRegVal X86Reg | InitStackVal Word64 deriving (Show, Eq)
data State = State {
  mem_data     :: BS.ByteString,
  blocks_queue :: [BlockAddr],
  blocks       :: AsmBlocks,
  regs         :: Registers,
  stack        :: Map Word64 AsmValue
} deriving (Show)

new_state :: BS.ByteString -> State
new_state contents = State {
    mem_data=contents, 
    blocks=empty,
    blocks_queue=[0x1DBF71A],
    regs=Registers {
      eax=InitRegVal X86RegEax,
      ebx=InitRegVal X86RegEbx,
      ecx=InitRegVal X86RegEcx,
      edx=InitRegVal X86RegEdx,
      esi=InitRegVal X86RegEsi,
      edi=InitRegVal X86RegEdi,
      ebp=InitRegVal X86RegEbp,
      esp=NumVal initial_stack_pos,
      eflags=0},
    stack=fromList([])}

initial_stack_pos = 1024 * 1024 :: Word64

fetch_reg_contents :: X86Reg -> State -> AsmValue
fetch_reg_contents X86RegEax state = eax $ regs state
fetch_reg_contents X86RegEbx state = ebx $ regs state
fetch_reg_contents X86RegEcx state = ecx $ regs state
fetch_reg_contents X86RegEdx state = edx $ regs state
fetch_reg_contents X86RegEsi state = esi $ regs state
fetch_reg_contents X86RegEdi state = edi $ regs state
fetch_reg_contents X86RegEbp state = ebp $ regs state
fetch_reg_contents X86RegEsp state = esp $ regs state

set_reg_contents :: X86Reg -> AsmValue -> State -> State
set_reg_contents X86RegEax val state = do
  let r = regs state
  state {regs = r { eax=val}}
set_reg_contents X86RegEbx val state = do
  let r = regs state
  state {regs = r { ebx=val}}
set_reg_contents X86RegEcx val state = do
  let r = regs state
  state {regs = r { ecx=val}}
set_reg_contents X86RegEdx val state = do
  let r = regs state
  state {regs = r { edx=val}}
set_reg_contents X86RegEsi val state = do
  let r = regs state
  state {regs = r { esi=val}}
set_reg_contents X86RegEdi val state = do
  let r = regs state
  state {regs = r { edi=val}}
set_reg_contents X86RegEbp val state = do
  let r = regs state
  state {regs = r { ebp=val}}
set_reg_contents X86RegEsp val state = do
  let r = regs state
  state {regs = r { esp=val}}

fetch_contents :: CsX86OpValue -> State -> AsmValue
fetch_contents (Imm value) _         = NumVal value
fetch_contents (Reg reg) state       = fetch_reg_contents reg state
fetch_contents (Mem mem) state       = if is_valid_stack_ref mem state
  then do
    let offset = stack_offset mem state
    let current_stack_pos = esp $ regs state
    let stack_map = stack state
    case fetch_reg_contents X86RegEsp state of
      NumVal current_stack_pos -> if List.elem (current_stack_pos + offset) $ keys stack_map
        then stack_map ! (current_stack_pos + offset)
        else InitStackVal (current_stack_pos + offset - initial_stack_pos)
      other -> other
  else error "bad mem type"

-- return state and is_mem_write
put_contents :: CsX86OpValue -> AsmValue -> State -> (State, Bool)
put_contents (Reg reg) val state = (set_reg_contents reg val state, False)
put_contents (Mem mem) val state = if is_valid_stack_ref mem state
  then do
    let offset = stack_offset mem state
    case fetch_reg_contents X86RegEsp state of
      NumVal current_stack_pos -> do
        let stack_map = stack state
        let new_stack = insert (current_stack_pos + offset) val stack_map
        (state { stack=new_stack }, False)
      otherwise -> error "Esp value invalid"
  else (state, True)

stack_offset :: X86OpMemStruct -> State -> Word64
stack_offset mem state = mem_index + mem_disp
  where
    mem_index = if index mem == X86RegInvalid then 0 else (get_int_value (index mem) state) * mem_scale
    mem_scale = fromIntegral(scale mem)::Word64
    mem_disp = fromIntegral(disp' mem)::Word64

is_valid_stack_ref :: X86OpMemStruct -> State -> Bool
is_valid_stack_ref mem state = (segment mem == X86RegInvalid) && (base mem == X86RegEsp) && ((index mem == X86RegInvalid) || (has_int_value (index mem) state))

get_int_value :: X86Reg -> State -> Word64
get_int_value reg state = case fetch_contents (Reg reg) state of
  NumVal val -> val
  otherwise  -> error "No int value"

has_int_value :: X86Reg -> State -> Bool
has_int_value reg state = case fetch_contents (Reg reg) state of
  NumVal _  -> True
  otherwise -> False

queue_block :: BlockAddr -> State -> State
queue_block bl_addr state = state { blocks_queue=bl_addr:(blocks_queue state)}

-- add an procces instruction to block
add_block_content :: InsnAddr -> ProccessedInsn -> State -> State
add_block_content addr insn state = do
  let i = (addr, insn)
  let bls = blocks state
  let bl = findWithDefault [] addr bls
  let ct = i : bl
  let xxx = insert addr ct bls
  state { blocks = xxx }

-- split current block at a position into multiple blocks
split_block :: Int -> AsmBlock -> AsmBlocks -> (AsmBlocks, BlockAddr)
split_block at block blocks = do
  let (old_bl, new_bl) = List.splitAt at block
  let ((oaddr, _):_) = old_bl
  let ((naddr, _):_) = new_bl
  let new_blocks = insert oaddr old_bl $ blocks
  (new_blocks, naddr)
