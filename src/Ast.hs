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

flagToBit :: X86Flag -> (Int, Int)

flagToBit flag = case flag of
  X86FlagCf -> (0,0)
  X86FlagPf -> (2,2)
  X86FlagAf -> (4,4)
  X86FlagZf -> (6,6)
  X86FlagSf -> (7,7)
  X86FlagTf -> (8,8)
  X86FlagIf -> (9,9)
  X86FlagDf -> (10,10)
  X86FlagOf -> (11,11)
  X86FlagIopl -> (12,13)
  X86FlagNt -> (14,14)
  X86FlagRf -> (16,16)
  X86FlagVm -> (17,17)
  X86FlagAc -> (18,18)
  X86FlagVif -> (19,19)
  X86FlagVip -> (20,20)
  X86FlagId -> (21,21)

-- A representation of a register as a list of indicies. Enables overlapping registers.

type CompoundReg = (Int, Int)

-- A map from X86Regs to locations in the register file

x86RegisterMap :: [(X86.X86Reg, CompoundReg)]

x86RegisterMap = [
-- 8-bit operands

  (X86RegAh, (1,1)), (X86RegBh, (9,9)), (X86RegCh, (17,17)), (X86RegDh, (25,25)),

  (X86RegAl, (0,0)), (X86RegBl, (8,8)), (X86RegCl, (16,16)), (X86RegDl, (24,24)),
  (X86RegDil, (32,32)), (X86RegSil, (40,40)), (X86RegBpl, (48,48)), (X86RegSpl, (56,56)),
  (X86RegR8b, (64,64)), (X86RegR9b, (72,72)), (X86RegR10b, (80,80)), (X86RegR11b, (88,88)),
  (X86RegR12b, (96,96)), (X86RegR13b, (104,104)), (X86RegR14b, (112,112)), (X86RegR15b, (120,120)),

-- 16-bit operands

  (X86RegAx, (0,1)), (X86RegBx, (8,9)), (X86RegCx, (16,17)), (X86RegDx, (24,25)),
  (X86RegDi, (32,33)), (X86RegSi, (40,41)), (X86RegBp, (48,49)), (X86RegSp, (56,57)),
  (X86RegR8w, (64,65)), (X86RegR9w, (72,73)), (X86RegR10w, (80,81)), (X86RegR11w, (88,89)),
  (X86RegR12w, (96,97)), (X86RegR13w, (104,105)), (X86RegR14w, (112,113)), (X86RegR15w, (120,121)),
  (X86RegIp, (184,185)),

-- 32-bit operands

  (X86RegEax, (0,3)), (X86RegEbx, (8,11)), (X86RegEcx, (16,19)), (X86RegEdx, (24,27)),
  (X86RegEdi, (32,35)), (X86RegEsi, (40,43)), (X86RegEbp, (48,51)), (X86RegEsp, (56,59)),
  (X86RegR8d, (64,67)), (X86RegR9d, (72,75)), (X86RegR10d, (80,83)), (X86RegR11d, (88,91)),
  (X86RegR12d, (96,99)), (X86RegR13d, (104,107)), (X86RegR14d, (112,115)), (X86RegR15d, (120,123)),
  (X86RegEip, (184,187)),

-- 64-bit operands

  (X86RegRax, (0,7)), (X86RegRbx, (8,15)), (X86RegRcx, (16,23)), (X86RegRdx, (24,31)),
  (X86RegRdi, (32,39)), (X86RegRsi, (40,47)), (X86RegRbp, (48,55)), (X86RegRsp, (56,63)),
  (X86RegR8, (64,71)), (X86RegR9, (72,79)), (X86RegR10, (80,87)), (X86RegR11, (88,95)),
  (X86RegR12, (96,103)), (X86RegR13, (104,111)), (X86RegR14, (112,119)), (X86RegR15, (120,127)),
  (X86RegRip, (184,191)),

-- Segment Registers

  (X86RegCs, (128,135)), (X86RegDs, (136,143)), (X86RegSs, (144,151)), (X86RegEs, (152,159)),
  (X86RegFs, (160,167)), (X86RegGs, (168,175)),

-- EFLAGS Register

  (X86RegEflags, (176,183))]

-- Convert an X86Reg to a CompoundReg

compoundReg :: X86.X86Reg -> CompoundReg

compoundReg reg = case lookup reg x86RegisterMap of
  Nothing -> error "X86 register could not be found in map."
  Just x -> x

-- Gets the stack register for the given processor mode

get_stack_reg :: [CsMode] -> CompoundReg
get_stack_reg modes =
  if elem CsMode16 modes then compoundReg X86RegSp
  else if elem CsMode32 modes then compoundReg X86RegEsp
  else if elem CsMode64 modes then compoundReg X86RegRsp
  else error "Processor modes underspecified."

-- Gets the instruction pointer for the given processor mode

get_insn_ptr :: [CsMode] -> CompoundReg
get_insn_ptr modes =
  if elem CsMode16 modes then compoundReg X86RegIp
  else if elem CsMode32 modes then compoundReg X86RegEip
  else if elem CsMode64 modes then compoundReg X86RegRip
  else error "Processor modes underspecified."

-- Gets the architecture size for the given processor mode

get_arch_size :: [CsMode] -> Int
get_arch_size modes =
  if elem CsMode16 modes then 2
  else if elem CsMode32 modes then 4
  else if elem CsMode64 modes then 8
  else error "Processor modes underspecified."

-- Gets the value of the specified compound register from the register file

getRegisterValue :: [Int] -> CompoundReg -> Int

getRegisterValue regFile (l, h) | l == h+1 = 0

getRegisterValue regFile (l, h) =
  (regFile !! l) + (shift (getRegisterValue regFile (l+1, h)) byte_size_bit)

-- Replace the given index of the given list with the given value

replace :: [a] -> Int -> a -> [a]

replace (_:xs) 0 val = val:xs

replace (x:xs) idx val = x:(replace xs (idx - 1) val)

-- Updates the given register file by putting the given value in the given register

update_reg_file :: [Int] -> CompoundReg -> Int -> [Int]

update_reg_file regs (l, h) _ | l == h+1 = regs

update_reg_file regs (l, h) val =
  update_reg_file (replace regs l (val .&. ((shift 1 byte_size_bit) - 1))) (l+1,h)
    (convert (shift ((convert val) :: Word) (-byte_size_bit)))

-- Gets the specified bytes from memory

getMemoryValue :: [(Int, Int)] -> [Int] -> Int

getMemoryValue _ [] = 0

getMemoryValue mem (b:bs) =
  case lookup b mem of
    Nothing -> error "Read attempted on uninitialized memory."
    Just x -> x + (shift (getMemoryValue mem bs) byte_size_bit)

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

