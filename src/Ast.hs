module Ast where

import Data.Word
import Data.List
import Hapstone.Internal.X86 as X86
import Hapstone.Internal.Capstone as Capstone
import Data.Bits
import Util

byte_size_bit :: Num a => a
byte_size_bit = 8
word_size_bit :: Num a => a
word_size_bit = 16
dword_size_bit = 32
qword_size_bit = 64
dqword_size_bit = 128
qqword_size_bit = 256
dqqword_size_bit = 512

reg_file_bytes :: Num a => a
reg_file_bytes = 192

data X86Flag =
    X86FlagCf | X86FlagPf | X86FlagAf | X86FlagZf | X86FlagSf | X86FlagTf | X86FlagIf
  | X86FlagDf | X86FlagOf | X86FlagIopl | X86FlagNt | X86FlagRf | X86FlagVm | X86FlagAc
  | X86FlagVif | X86FlagVip | X86FlagId deriving (Eq, Show)

-- A representation of a register as a list of indicies. Enables overlapping registers.

type CompoundReg = (Int, Int)

-- A map from X86Regs to locations in the register file

x86RegisterMap :: [(X86.X86Reg, CompoundReg)]

x86RegisterMap = [
-- 8-bit operands

  (X86RegAh, b(1,1)), (X86RegBh, b(9,9)), (X86RegCh, b(17,17)), (X86RegDh, b(25,25)),

  (X86RegAl, b(0,0)), (X86RegBl, b(8,8)), (X86RegCl, b(16,16)), (X86RegDl, b(24,24)),
  (X86RegDil, b(32,32)), (X86RegSil, b(40,40)), (X86RegBpl, b(48,48)), (X86RegSpl, b(56,56)),
  (X86RegR8b, b(64,64)), (X86RegR9b, b(72,72)), (X86RegR10b, b(80,80)), (X86RegR11b, b(88,88)),
  (X86RegR12b, b(96,96)), (X86RegR13b, b(104,104)), (X86RegR14b, b(112,112)), (X86RegR15b, b(120,120)),

-- 16-bit operands

  (X86RegAx, b(0,1)), (X86RegBx, b(8,9)), (X86RegCx, b(16,17)), (X86RegDx, b(24,25)),
  (X86RegDi, b(32,33)), (X86RegSi, b(40,41)), (X86RegBp, b(48,49)), (X86RegSp, b(56,57)),
  (X86RegR8w, b(64,65)), (X86RegR9w, b(72,73)), (X86RegR10w, b(80,81)), (X86RegR11w, b(88,89)),
  (X86RegR12w, b(96,97)), (X86RegR13w, b(104,105)), (X86RegR14w, b(112,113)), (X86RegR15w, b(120,121)),
  (X86RegIp, b(184,185)),

-- 32-bit operands

  (X86RegEax, b(0,3)), (X86RegEbx, b(8,11)), (X86RegEcx, b(16,19)), (X86RegEdx, b(24,27)),
  (X86RegEdi, b(32,35)), (X86RegEsi, b(40,43)), (X86RegEbp, b(48,51)), (X86RegEsp, b(56,59)),
  (X86RegR8d, b(64,67)), (X86RegR9d, b(72,75)), (X86RegR10d, b(80,83)), (X86RegR11d, b(88,91)),
  (X86RegR12d, b(96,99)), (X86RegR13d, b(104,107)), (X86RegR14d, b(112,115)), (X86RegR15d, b(120,123)),
  (X86RegEip, b(184,187)),

-- 64-bit operands

  (X86RegRax, b(0,7)), (X86RegRbx, b(8,15)), (X86RegRcx, b(16,23)), (X86RegRdx, b(24,31)),
  (X86RegRdi, b(32,39)), (X86RegRsi, b(40,47)), (X86RegRbp, b(48,55)), (X86RegRsp, b(56,63)),
  (X86RegR8, b(64,71)), (X86RegR9, b(72,79)), (X86RegR10, b(80,87)), (X86RegR11, b(88,95)),
  (X86RegR12, b(96,103)), (X86RegR13, b(104,111)), (X86RegR14, b(112,119)), (X86RegR15, b(120,127)),
  (X86RegRip, b(184,191)),

-- Segment Registers

  (X86RegCs, b(128,135)), (X86RegDs, b(136,143)), (X86RegSs, b(144,151)), (X86RegEs, b(152,159)),
  (X86RegFs, b(160,167)), (X86RegGs, b(168,175))

-- EFLAGS Register

  {-,(X86RegEflags, b(176,183))-}] where b(l,h) = (l*byte_size_bit,h*byte_size_bit)

-- Convert an X86Reg to a CompoundReg

fromX86Reg :: X86.X86Reg -> CompoundReg

fromX86Reg reg = case lookup reg x86RegisterMap of
  Nothing -> error "X86 register could not be found in map."
  Just x -> x

-- A map from X86Flags to locations in the register file

x86FlagMap :: [(X86Flag, CompoundReg)]

x86FlagMap = [(X86FlagCf, c(176,0,0)), (X86FlagPf, c(176,2,2)),
  (X86FlagAf, c(176,4,4)), (X86FlagZf, c(176,6,6)), (X86FlagSf, c(176,7,7)),
  (X86FlagTf, c(176,8,8)), (X86FlagIf, c(176,9,9)), (X86FlagDf, c(176,10,10)),
  (X86FlagOf, c(176,11,11)), (X86FlagIopl, c(176,12,13)), (X86FlagNt, c(176,14,14)),
  (X86FlagRf, c(176,16,16)), (X86FlagVm, c(176,17,17)), (X86FlagAc, c(176,18,18)),
  (X86FlagVif, c(176,19,19)), (X86FlagVip, c(176,20,20)), (X86FlagId, c(176,21,21))]
  
  where c(b,l,h) = (b*byte_size_bit+l,b*byte_size_bit+h)

-- Convert an X86Reg to a CompoundReg

fromX86Flag :: X86Flag -> CompoundReg

fromX86Flag reg = case lookup reg x86FlagMap of
  Nothing -> error "X86 flag could not be found in map."
  Just x -> x

-- Gets the size of the given register in bits

getRegSize :: CompoundReg -> Int

getRegSize (l, h) = h - l

-- Gets the stack register for the given processor mode

get_stack_reg :: [CsMode] -> CompoundReg

get_stack_reg modes =
  if elem CsMode16 modes then fromX86Reg X86RegSp
  else if elem CsMode32 modes then fromX86Reg X86RegEsp
  else if elem CsMode64 modes then fromX86Reg X86RegRsp
  else error "Processor modes underspecified."

-- Gets the instruction pointer for the given processor mode

get_insn_ptr :: [CsMode] -> CompoundReg

get_insn_ptr modes =
  if elem CsMode16 modes then fromX86Reg X86RegIp
  else if elem CsMode32 modes then fromX86Reg X86RegEip
  else if elem CsMode64 modes then fromX86Reg X86RegRip
  else error "Processor modes underspecified."

-- Gets the architecture size for the given processor mode

get_arch_size :: [CsMode] -> Int

get_arch_size modes =
  if elem CsMode16 modes then 2
  else if elem CsMode32 modes then 4
  else if elem CsMode64 modes then 8
  else error "Processor modes underspecified."

-- Get the given bit of the integer

getBit :: Int -> Int -> Int

getBit value bit = if testBit value bit then 1 else 0

-- Gets the value of the specified compound register from the register file

getRegisterValue :: [Int] -> CompoundReg -> Int

getRegisterValue regFile (l, h) | l > h = 0

-- If the register's bits are multiples of byte_size_bit, access them using a list index

getRegisterValue regFile (l, h) | (mod l byte_size_bit == 0) && (mod h byte_size_bit == 0) =
  let l_byte = div l byte_size_bit
      upper_bytes = getRegisterValue regFile (l+byte_size_bit, h)
  in (regFile !! l_byte) + shift upper_bytes byte_size_bit

-- Otherwise get the register value from the register file one bit at a time

getRegisterValue regFile (l, h) =
  let l_byte = div l byte_size_bit
      l_bit = mod l byte_size_bit
      upper_bits = getRegisterValue regFile (l+1, h)
  in getBit (regFile !! l_byte) l_bit + shift upper_bits 1

-- Replace the given index of the given list with the given value

replace :: [a] -> Int -> a -> [a]

replace (_:xs) 0 val = val:xs

replace (x:xs) idx val = x:(replace xs (idx - 1) val)

-- Set the given bit of the integer to the given value

assignBit :: Int -> Int -> Int -> Int

assignBit value bit state =
  (value .&. (complement (shift 1 bit))) .|. (shift state bit)

-- Updates the given register file by putting the given value in the given register

update_reg_file :: [Int] -> CompoundReg -> Int -> [Int]

update_reg_file regs (l, h) _ | l > h = regs

-- If the register's bits are multiples of byte_size_bit, access them using a list index

update_reg_file regs (l, h) val | (mod l byte_size_bit == 0) && (mod h byte_size_bit == 0) =
  let l_byte = div l byte_size_bit
      val_byte0 = val .&. (bit byte_size_bit - 1)
      upper_bytes = convert (shift ((convert val) :: Word) (-byte_size_bit))
  in update_reg_file (replace regs l_byte val_byte0) (l+byte_size_bit,h) upper_bytes

-- Otherwise put the given value into the register file one bit at a time

update_reg_file regs (l, h) val =
  let l_byte = div l byte_size_bit
      l_rounded = l_byte * byte_size_bit
      effective_reg = (l_rounded, l_rounded + byte_size_bit)
      current_val = getRegisterValue regs effective_reg
      new_val = assignBit current_val (mod l byte_size_bit) (val .&. 1)
  in update_reg_file (replace regs l_byte new_val) (l+1,h) (shift val (-1))

-- Gets the specified bytes from memory

getMemoryValue :: [(Int, Int)] -> [Int] -> Maybe Int

getMemoryValue _ [] = Just 0

getMemoryValue mem (b:bs) =
  case (lookup b mem, getMemoryValue mem bs) of
    (Just x, Just y) -> Just (x + shift y byte_size_bit)
    _ -> Nothing

-- Get the register values from the register file

getRegisterValues regFile =
  map (\(x, y) -> (x, getRegisterValue regFile y)) x86RegisterMap

-- Checks if a register is a subregister of another register

isSubregisterOf :: CompoundReg -> CompoundReg -> Bool

isSubregisterOf (childL, childH) (parentL, parentH) =
  (parentL <= childL) && (parentH >= childH)

-- Checks if the given register is a segment register

is_segment_reg :: X86.X86Reg -> Bool

is_segment_reg reg = elem reg [X86RegCs, X86RegDs, X86RegSs, X86RegEs, X86RegFs, X86RegGs]

-- Checks if the given register is a control register

is_control_reg :: X86.X86Reg -> Bool

is_control_reg reg = elem reg
  [X86RegCr0, X86RegCr1, X86RegCr2, X86RegCr3, X86RegCr4, X86RegCr5, X86RegCr6,
  X86RegCr7, X86RegCr8, X86RegCr9, X86RegCr10, X86RegCr11, X86RegCr12, X86RegCr13,
  X86RegCr14, X86RegCr15]

data Expr =
    BvaddExpr Expr Expr
  | BvandExpr Expr Expr
  | BvashrExpr Expr Expr
  | BvlshrExpr Expr Expr
  | BvmulExpr Expr Expr
  | BvnandExpr Expr Expr
  | BvnegExpr Expr
  | BvnorExpr Expr Expr
  | BvnotExpr Expr
  | BvorExpr Expr Expr
  | BvrolExpr Expr Expr
  | BvrorExpr Expr Expr -- can lit
  | BvsdivExpr Expr Expr
  | BvsgeExpr Expr Expr
  | BvsgtExpr Expr Expr
  | BvshlExpr Expr Expr
  | BvsleExpr Expr Expr
  | BvsltExpr Expr Expr
  | BvsmodExpr Expr Expr
  | BvsremExpr Expr Expr
  | BvsubExpr Expr Expr
  | BvudivExpr Expr Expr
  | BvugeExpr Expr Expr
  | BvugtExpr Expr Expr
  | BvuleExpr Expr Expr
  | BvultExpr Expr Expr
  | BvuremExpr Expr Expr
  | BvxnorExpr Expr Expr
  | BvxorExpr Expr Expr
  | BvExpr Int Int
  | CompoundExpr -- ! `[<expr1> <expr2> <expr3> ...]` node
  | ConcatExpr [Expr]
  | DecimalExpr Int --float?
  | DeclareExpr --wtf?
  | DistinctExpr Expr Expr
  | EqualExpr Expr Expr
  | ExtractExpr Int Int Expr -- ! `((_ extract <high> <low>) <expr>)` node
  | ReplaceExpr Int Int Expr Expr
  | IffExpr Expr Expr -- ! `(iff <expr1> <expr2>)`
  | IteExpr Expr Expr Expr -- ! `(ite <ifExpr> <thenExpr> <elseExpr>)`
  | LandExpr Expr Expr
  | LetExpr String Expr Expr
  | LnotExpr Expr
  | LorExpr Expr Expr
  | ReferenceExpr --fix
  | StringExpr String
  | SxExpr Int Expr
  | VariableExpr
  | ZxExpr Int Expr
  | UndefinedExpr -- The undefined value
  | Load Int Expr
  | GetReg CompoundReg
  deriving (Eq, Show)

data Stmt =
    Store Int Expr Expr
  | SetReg CompoundReg Expr
  deriving (Eq, Show)

