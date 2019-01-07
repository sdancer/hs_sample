module Asm where

import qualified Data.ByteString            as BS
import           Data.Map.Strict
import           Data.Word
import           Hapstone.Capstone
import           Hapstone.Internal.Capstone as Capstone
import           Hapstone.Internal.X86      as X86

data Registers = Registers {
  eax :: Maybe AsmValue,
  ebx :: Maybe AsmValue,
  ecx :: Maybe AsmValue,
  edx :: Maybe AsmValue,
  esi :: Maybe AsmValue,
  edi :: Maybe AsmValue,
  ebp :: Maybe AsmValue,
  esp :: Maybe AsmValue
} deriving (Show)
data ProccessedInsn = NormalInsn CsInsn | JunkInsn String deriving (Show)
type InsnAddr = Word64
type BlockAddr = Word64
type AsmBlock = [(InsnAddr, ProccessedInsn)]
type AsmBlocks = Map BlockAddr AsmBlock
data AbstractOp = AbstractOp {
  optype :: [Char],
  op1    :: X86OpType,
  op2    :: X86OpType
} deriving (Show)
data AsmValue = NumVal Word64 | StrVal [Char]  | AbsVal AbstractOp deriving (Show)
data State = State {
  mem_data     :: BS.ByteString,
  blocks_queue :: [BlockAddr],
  blocks       :: AsmBlocks,
  regs         :: Registers,
  stack        :: Map Word32 AsmValue
} deriving (Show)

new_state :: BS.ByteString -> State
new_state contents = State {mem_data=contents, blocks=empty, blocks_queue=[0x1DBF71A],regs=Registers {
  eax=Nothing,
  ebx=Nothing,
  ecx=Nothing,
  edx=Nothing,
  esi=Nothing,
  edi=Nothing,
  ebp=Nothing,
  esp=Nothing}, stack=fromList([])}

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

fetch_contents :: State -> CsX86OpValue -> Maybe AsmValue
fetch_contents _ (X86.Imm value) = Just $ NumVal value
fetch_contents state (Reg X86.X86RegEax) = eax $ regs state
fetch_contents state (Reg X86.X86RegEbx) = ebx $ regs state
fetch_contents state (Reg X86.X86RegEcx) = ecx $ regs state
fetch_contents state (Reg X86.X86RegEsi) = esi $ regs state
fetch_contents state (Reg X86.X86RegEdi) = edi $ regs state
fetch_contents state (Reg X86.X86RegEbp) = ebp $ regs state
fetch_contents state (Reg X86.X86RegEsp) = esp $ regs state
fetch_contents state (X86.Mem mem) = Nothing


is_valid_stack_ref :: State -> X86OpMemStruct -> Bool
is_valid_stack_ref state mem = (segment mem == X86.X86RegEax) && (base mem == X86.X86RegEsp) && ((index mem == X86.X86RegEax) || (has_int_value state $ index mem))

has_int_value :: State -> X86Reg -> Bool
has_int_value state reg = case fetch_contents state $ Reg reg of
  Just (NumVal _) -> True
  otherwise -> False

-- Virtual proccess instruction
vproc :: State -> CsInsn -> State
vproc state insn = state
