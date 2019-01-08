module Asm where

import qualified Data.ByteString            as BS
import qualified Data.List                  as List
import           Data.Map.Strict
import           Data.Word
import           Hapstone.Capstone
import           Hapstone.Internal.Capstone as Capstone
import           Hapstone.Internal.X86      as X86

data Registers = Registers {
  eax :: AsmValue,
  ebx :: AsmValue,
  ecx :: AsmValue,
  edx :: AsmValue,
  esi :: AsmValue,
  edi :: AsmValue,
  ebp :: AsmValue,
  esp :: AsmValue
} deriving (Show)
data ProccessedInsn = Insn CsInsn | Skip CsInsn | Break CsInsn deriving (Show)
type InsnAddr = Word64
type BlockAddr = Word64
type AsmBlock = [(InsnAddr, ProccessedInsn)]
type AsmBlocks = Map BlockAddr AsmBlock
data AbstractOp = AbstractOp {
  optype :: [Char],
  op1    :: X86OpType,
  op2    :: X86OpType
} deriving (Show)
data AsmValue = NumVal Word64 | StrVal [Char]  | AbsVal AbstractOp | InitRegVal X86Reg | InitStackVal Word64 deriving (Show)
data State = State {
  mem_data     :: BS.ByteString,
  blocks_queue :: [BlockAddr],
  blocks       :: AsmBlocks,
  regs         :: Registers,
  stack        :: Map Word64 AsmValue
} deriving (Show)

new_state :: BS.ByteString -> State
new_state contents = State {mem_data=contents, blocks=empty, blocks_queue=[0x1DBF71A],regs=Registers {
  eax=InitRegVal X86RegEax,
  ebx=InitRegVal X86RegEbx,
  ecx=InitRegVal X86RegEcx,
  edx=InitRegVal X86RegEdx,
  esi=InitRegVal X86RegEsi,
  edi=InitRegVal X86RegEdi,
  ebp=InitRegVal X86RegEbp,
  esp=NumVal initial_stack_pos}, stack=fromList([])}

initial_stack_pos = 1024 * 1024 :: Word64

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

-- calc next instruction address
next_addr :: CsInsn -> InsnAddr
next_addr insn = (address insn) + insn_size
  where
    insn_size = fromIntegral(length $ bytes insn)::InsnAddr

fetch_reg_contents :: State -> X86Reg -> AsmValue
fetch_reg_contents state X86RegEax = eax $ regs state
fetch_reg_contents state X86RegEbx = ebx $ regs state
fetch_reg_contents state X86RegEcx = ecx $ regs state
fetch_reg_contents state X86RegEdx = edx $ regs state
fetch_reg_contents state X86RegEsi = esi $ regs state
fetch_reg_contents state X86RegEdi = edi $ regs state
fetch_reg_contents state X86RegEbp = ebp $ regs state
fetch_reg_contents state X86RegEsp = esp $ regs state

set_reg_contents :: State -> X86Reg -> AsmValue -> State
set_reg_contents state X86RegEax val = do
  let r = regs state
  state {regs = r { eax=val}}
set_reg_contents state X86RegEbx val = do
  let r = regs state
  state {regs = r { ebx=val}}
set_reg_contents state X86RegEcx val = do
  let r = regs state
  state {regs = r { ecx=val}}
set_reg_contents state X86RegEdx val = do
  let r = regs state
  state {regs = r { edx=val}}
set_reg_contents state X86RegEsi val = do
  let r = regs state
  state {regs = r { esi=val}}
set_reg_contents state X86RegEdi val = do
  let r = regs state
  state {regs = r { edi=val}}
set_reg_contents state X86RegEbp val = do
  let r = regs state
  state {regs = r { ebp=val}}
set_reg_contents state X86RegEsp val = do
  let r = regs state
  state {regs = r { esp=val}}

fetch_contents :: State -> CsX86OpValue -> AsmValue
fetch_contents _ (Imm value)         = NumVal value
fetch_contents state (Reg reg)           = fetch_reg_contents state reg
fetch_contents state (Mem mem)       = if is_valid_stack_ref state mem
  then do
    let offset = stack_offset state mem
    let current_stack_pos = esp $ regs state
    let stack_map = stack state
    case fetch_reg_contents state X86RegEsp of
      NumVal current_stack_pos -> if List.elem (current_stack_pos + offset) $ keys stack_map
        then stack_map ! (current_stack_pos + offset)
        else InitStackVal (current_stack_pos + offset - initial_stack_pos)
      other -> other
  else error "bad mem type"

-- return state and is_mem_write
put_contents :: State -> CsX86OpValue -> AsmValue -> (State, Bool)
put_contents state (Reg reg) val = (set_reg_contents state reg val, False)
put_contents state (Mem mem) val = if is_valid_stack_ref state mem
  then do
    let offset = stack_offset state mem
    case fetch_reg_contents state X86RegEsp of
      NumVal current_stack_pos -> do
        let stack_map = stack state
        let new_stack = insert (current_stack_pos + offset) val stack_map
        (state { stack=new_stack }, False)
      otherwise -> error "Esp value invalid"
  else (state, True)

stack_offset :: State -> X86OpMemStruct -> Word64
stack_offset state mem = mem_index + mem_disp
  where
    mem_index = if index mem == X86RegInvalid then 0 else (get_int_value state $ index mem) * mem_scale
    mem_scale = fromIntegral(scale mem)::Word64
    mem_disp = fromIntegral(disp' mem)::Word64

is_valid_stack_ref :: State -> X86OpMemStruct -> Bool
is_valid_stack_ref state mem = (segment mem == X86RegInvalid) && (base mem == X86RegEsp) && ((index mem == X86RegInvalid) || (has_int_value state $ index mem))

get_int_value :: State -> X86Reg -> Word64
get_int_value state reg = case fetch_contents state $ Reg reg of
  NumVal val -> val
  otherwise         -> error "No int value"

has_int_value :: State -> X86Reg -> Bool
has_int_value state reg = case fetch_contents state $ Reg reg of
  NumVal _ -> True
  otherwise       -> False

-- Virtual proccess instruction
vproc :: State -> CsInsn -> State
vproc state insn = state
