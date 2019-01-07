module Asm where

import qualified Data.ByteString            as BS
import           Data.Map.Strict
import           Data.Word
import           Hapstone.Capstone
import           Hapstone.Internal.Capstone as Capstone
import           Hapstone.Internal.X86      as X86

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
type InsnAddr = Word64
type BlockAddr = Word64
type AsmBlock = [(InsnAddr, ProccessedInsn)]
type AsmBlocks = Map BlockAddr AsmBlock
data AbstractOp = AbstractOp {
  optype :: [Char],
  op1    :: X86OpType,
  op2    :: X86OpType
} deriving (Show)
data StackValue = IntVal Int | StrVal [Char]  | AbsVal AbstractOp deriving (Show)
data State = State {
  mem_data     :: BS.ByteString,
  blocks_queue :: [BlockAddr],
  blocks       :: AsmBlocks,
  regs         :: Registers,
  stack        :: Map Word32 StackValue
} deriving (Show)

-- Virtual proccess instruction
vproc :: State -> CsInsn -> State
vproc state insn = state
