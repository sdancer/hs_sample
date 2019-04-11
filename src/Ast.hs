module Ast where

import Data.Word
import Data.List
import Hapstone.Internal.X86 as X86
import Hapstone.Internal.Capstone as Capstone
import Data.Bits
import Util
import BitVector

byte_size_bit :: Num a => a
byte_size_bit = 8
word_size_bit :: Num a => a
word_size_bit = 16
dword_size_bit = 32
qword_size_bit = 64
dqword_size_bit = 128
qqword_size_bit = 256
dqqword_size_bit = 512

reg_file_bits :: Num a => a
reg_file_bits = 192 * 8

data X86Flag =
    X86FlagCf | X86FlagPf | X86FlagAf | X86FlagZf | X86FlagSf | X86FlagTf | X86FlagIf
  | X86FlagDf | X86FlagOf | X86FlagIopl | X86FlagNt | X86FlagRf | X86FlagVm | X86FlagAc
  | X86FlagVif | X86FlagVip | X86FlagId deriving (Eq, Show)

-- A representation of a register as a pair indicating the lower (inclusive) and upper bit
-- (exclusive) indicies. Enables overlapping registers.

type CompoundReg = (Int, Int)

-- A map from X86Regs to locations in the register file

x86RegisterMap :: [(X86.X86Reg, CompoundReg)]

x86RegisterMap = [
-- 8-bit operands

  (X86RegAh, b(1,2)), (X86RegBh, b(9,10)), (X86RegCh, b(17,18)), (X86RegDh, b(25,26)),

  (X86RegAl, b(0,1)), (X86RegBl, b(8,9)), (X86RegCl, b(16,17)), (X86RegDl, b(24,25)),
  (X86RegDil, b(32,33)), (X86RegSil, b(40,41)), (X86RegBpl, b(48,49)), (X86RegSpl, b(56,57)),
  (X86RegR8b, b(64,65)), (X86RegR9b, b(72,73)), (X86RegR10b, b(80,81)), (X86RegR11b, b(88,89)),
  (X86RegR12b, b(96,97)), (X86RegR13b, b(104,105)), (X86RegR14b, b(112,113)), (X86RegR15b, b(120,121)),

-- 16-bit operands

  (X86RegAx, b(0,2)), (X86RegBx, b(8,10)), (X86RegCx, b(16,18)), (X86RegDx, b(24,26)),
  (X86RegDi, b(32,34)), (X86RegSi, b(40,42)), (X86RegBp, b(48,50)), (X86RegSp, b(56,58)),
  (X86RegR8w, b(64,66)), (X86RegR9w, b(72,74)), (X86RegR10w, b(80,82)), (X86RegR11w, b(88,90)),
  (X86RegR12w, b(96,98)), (X86RegR13w, b(104,106)), (X86RegR14w, b(112,114)), (X86RegR15w, b(120,122)),
  (X86RegIp, b(184,186)),

-- 32-bit operands

  (X86RegEax, b(0,4)), (X86RegEbx, b(8,12)), (X86RegEcx, b(16,20)), (X86RegEdx, b(24,28)),
  (X86RegEdi, b(32,36)), (X86RegEsi, b(40,44)), (X86RegEbp, b(48,52)), (X86RegEsp, b(56,60)),
  (X86RegR8d, b(64,68)), (X86RegR9d, b(72,76)), (X86RegR10d, b(80,84)), (X86RegR11d, b(88,92)),
  (X86RegR12d, b(96,100)), (X86RegR13d, b(104,108)), (X86RegR14d, b(112,116)), (X86RegR15d, b(120,124)),
  (X86RegEip, b(184,188)),

-- 64-bit operands

  (X86RegRax, b(0,8)), (X86RegRbx, b(8,16)), (X86RegRcx, b(16,24)), (X86RegRdx, b(24,32)),
  (X86RegRdi, b(32,40)), (X86RegRsi, b(40,48)), (X86RegRbp, b(48,56)), (X86RegRsp, b(56,64)),
  (X86RegR8, b(64,72)), (X86RegR9, b(72,80)), (X86RegR10, b(80,88)), (X86RegR11, b(88,96)),
  (X86RegR12, b(96,104)), (X86RegR13, b(104,112)), (X86RegR14, b(112,120)), (X86RegR15, b(120,128)),
  (X86RegRip, b(184,192)),

-- Segment Registers

  (X86RegCs, b(128,136)), (X86RegDs, b(136,144)), (X86RegSs, b(144,152)), (X86RegEs, b(152,160)),
  (X86RegFs, b(160,168)), (X86RegGs, b(168,176))

-- EFLAGS Register

  {-,(X86RegEflags, b(176,184))-}] where b(l,h) = (l*byte_size_bit,h*byte_size_bit)

-- Convert an X86Reg to a CompoundReg

fromX86Reg :: X86.X86Reg -> CompoundReg

fromX86Reg reg = case lookup reg x86RegisterMap of
  Nothing -> error "X86 register could not be found in map."
  Just x -> x

-- A map from X86Flags to locations in the register file

x86FlagMap :: [(X86Flag, CompoundReg)]

x86FlagMap = [(X86FlagCf, c(176,0,1)), (X86FlagPf, c(176,2,3)),
  (X86FlagAf, c(176,4,5)), (X86FlagZf, c(176,6,7)), (X86FlagSf, c(176,7,8)),
  (X86FlagTf, c(176,8,9)), (X86FlagIf, c(176,9,10)), (X86FlagDf, c(176,10,11)),
  (X86FlagOf, c(176,11,12)), (X86FlagIopl, c(176,12,14)), (X86FlagNt, c(176,14,15)),
  (X86FlagRf, c(176,16,17)), (X86FlagVm, c(176,17,18)), (X86FlagAc, c(176,18,19)),
  (X86FlagVif, c(176,19,20)), (X86FlagVip, c(176,20,21)), (X86FlagId, c(176,21,22))]
  
  where c(b,l,h) = (b*byte_size_bit+l,b*byte_size_bit+h)

-- Convert an X86Reg to a CompoundReg

fromX86Flag :: X86Flag -> CompoundReg

fromX86Flag reg = case lookup reg x86FlagMap of
  Nothing -> error "X86 flag could not be found in map."
  Just x -> x

-- Gets the size of the given register in bits

getRegisterSize :: CompoundReg -> Int

getRegisterSize (l, h) = h - l

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

get_arch_byte_size :: [CsMode] -> Int

get_arch_byte_size modes =
  if elem CsMode16 modes then 2
  else if elem CsMode32 modes then 4
  else if elem CsMode64 modes then 8
  else error "Processor modes underspecified."

get_arch_bit_size :: [CsMode] -> Int

get_arch_bit_size = (* 8) . get_arch_byte_size

-- Adds the given register to the given list taking care to combine those that overlap

addRegister :: [CompoundReg] -> CompoundReg -> [CompoundReg]

addRegister regs reg =
  let tryCombine ((l1,h1):rst) (l2,h2) =
        if l2 <= h1 then (l1, max h1 h2):rst else (l2,h2):(l1,h1):rst
  in foldl tryCombine [(0,0)] (sort (reg:regs))

-- Removes the given register from the given list taking care of overlaps

removeRegister :: [CompoundReg] -> CompoundReg -> [CompoundReg]

removeRegister regs (l1,h1) =
  -- Tries to remove register 1 from register 2
  let tryRemove (l2,h2) =
        -- If register 1 overlaps and is to the left of register 2
        if l1 <= l2 && l2 <= h1 && h1 <= h2 then [(h1,h2)]
        -- If register 1 is completely contained by register 2
        else if l2 <= l1 && h1 <= h2 then [(l2,l1),(h1,h2)]
        -- If register 1 overlaps and is to the right of register 2
        else if l2 <= l1 && l1 <= h2 && h2 <= h1 then [(l2,l1)]
        -- If register 1 completely contains register 2
        else if l1 <= l2 && h2 <= h1 then []
        -- Otherwise the registers are disjoint
        else [(l2,h2)]
  in foldl (++) [] (map tryRemove regs)

-- Checks if a register is a subregister of another register

isSubregisterOf :: CompoundReg -> CompoundReg -> Bool

isSubregisterOf (childL, childH) (parentL, parentH) = (parentL <= childL) && (parentH >= childH)

-- Gets the largest register containing this register

getRootRegister :: CompoundReg -> CompoundReg

getRootRegister reg = foldl (\x y -> if isSubregisterOf x y then y else x) reg (snd $ unzip x86RegisterMap)

-- Subtracts one register from another. Useful for obtaining a register's range relative to another register

registerSub :: CompoundReg -> CompoundReg -> CompoundReg

registerSub (l1, h1) (l2, h2) = (l1-l2, h1-l2)

-- Checks if the given register is a segment register

is_segment_reg :: X86.X86Reg -> Bool

is_segment_reg reg = elem reg [X86RegCs, X86RegDs, X86RegSs, X86RegEs, X86RegFs, X86RegGs]

-- Checks if the given register is a control register

is_control_reg :: X86.X86Reg -> Bool

is_control_reg reg = elem reg
  [X86RegCr0, X86RegCr1, X86RegCr2, X86RegCr3, X86RegCr4, X86RegCr5, X86RegCr6,
  X86RegCr7, X86RegCr8, X86RegCr9, X86RegCr10, X86RegCr11, X86RegCr12, X86RegCr13,
  X86RegCr14, X86RegCr15]

-- The expressions that the machine code will be lifted to. Expressions do not modify
-- context but are composable.

data Expr =
  -- Literal bit-vector. The size of this expression equals that of the bit-vector.
  BvExpr BitVector
  
  -- The following operations take and return equally sized operands
  | BvaddExpr Expr Expr -- Add
  | BvandExpr Expr Expr -- Bitwise and
  | BvashrExpr Expr Expr -- Arithmetic shift right
  | BvlshrExpr Expr Expr -- Logical shift right
  | BvmulExpr Expr Expr -- Multiply
  | BvnandExpr Expr Expr -- Bitwise nand
  | BvnegExpr Expr -- Negate
  | BvnorExpr Expr Expr -- Bitwise nor
  | BvnotExpr Expr -- Bitwise not
  | BvorExpr Expr Expr -- Bitwise or
  | BvrolExpr Expr Expr -- Rotate left
  | BvrorExpr Expr Expr -- Rotate right
  | BvsdivExpr Expr Expr -- Signed division
  | BvsgeExpr Expr Expr -- Signed greater than or equal
  | BvsgtExpr Expr Expr -- Signed greater than
  | BvshlExpr Expr Expr -- Shift left
  | BvsleExpr Expr Expr -- Signed less than or equal
  | BvsltExpr Expr Expr -- Signed less than
  | BvsmodExpr Expr Expr -- Signed modulo
  | BvsremExpr Expr Expr -- Signed remainder
  | BvsubExpr Expr Expr -- Subtraction
  | BvudivExpr Expr Expr -- Unsigned division
  | BvugeExpr Expr Expr -- Unsigned greater than or equal
  | BvugtExpr Expr Expr -- Unsigned greater than
  | BvuleExpr Expr Expr -- Unsigned less than or equal
  | BvultExpr Expr Expr -- Unsigned less than
  | BvuremExpr Expr Expr -- Unsigned remainder
  | BvxnorExpr Expr Expr -- Bitwise xnor
  | BvxorExpr Expr Expr -- Bitwise xor
  | EqualExpr Expr Expr -- Returns 1 is equal, 0 otherwise
  | IteExpr Expr Expr Expr -- If first expression is non-zero then return second, otherwise third
  | LandExpr Expr Expr -- Logical and
  | LnotExpr Expr -- Logical not
  | LorExpr Expr Expr -- Logical or
  
  -- Takes in order the low bit index (inclusive), the high bit index (exclusive), and the
  -- expression from which to extract. Size of resulting expression is the difference
  -- between the indicies.
  | ExtractExpr Int Int Expr
  -- Takes in order the low bit index (inclusive), the expression whose part starting at
  -- the aforementioned bit is to be replaced, and the expression that is to be used as
  -- the replacement. Size of returned expression equals that of first supplied expression.
  | ReplaceExpr Int Expr Expr
  -- Takes in order the size of the expression that is being referenced, and the
  -- identifier of the expression being reference. Size of this expression equals the size
  -- of the expression being referenced.
  | ReferenceExpr Int Int
  -- Sign extends the given expression by the given amount. Size of this expression equals
  -- the sum of the given amount and the size of the given expression.
  | SxExpr Int Expr
  -- Zero extends the given expression by the given amount. Size of this expression equals
  -- the sum of the given amount and the size of the given expression.
  | ZxExpr Int Expr
  -- An undefined expression of the given size.
  | UndefinedExpr Int
  -- Takes in order the number of bytes to extract from memory, and the address in memory
  -- from which to obtain data. Size of this expression equals the number of bits to be
  -- extracted from memory.
  | Load Int Expr
  -- Obtains the value of the given register. The size of this expression equals the size,
  -- in bits, of the register.
  | GetReg CompoundReg
  
  | CompoundExpr -- ! `[<expr1> <expr2> <expr3> ...]` node
  | ConcatExpr [Expr]
  | DecimalExpr Int --float?
  | DeclareExpr --wtf?
  | DistinctExpr Expr Expr
  | IffExpr Expr Expr -- ! `(iff <expr1> <expr2>)`
  | LetExpr String Expr Expr
  | StringExpr String
  | VariableExpr
  deriving (Eq, Show)

-- The statements that the machine code will be lifted to. Statements modify context and
-- are not composable.

data Stmt =
    Store Expr Expr
  | SetReg CompoundReg Expr
  deriving (Eq, Show)

-- Same as a statement but with an integer label to identify it. This type is necessary
-- for the purposes of referring to past expressions

type LbldStmt = (Int, Stmt)

getExprSize :: Expr -> Int

getExprSize (BvaddExpr a b) = getExprSize a

getExprSize (BvandExpr a b) = getExprSize a

getExprSize (BvashrExpr a b) = getExprSize a

getExprSize (BvlshrExpr a b) = getExprSize a

getExprSize (BvnandExpr a b) = getExprSize a

getExprSize (BvnegExpr a) = getExprSize a

getExprSize (BvnorExpr a b) = getExprSize a

getExprSize (BvnotExpr a) = getExprSize a

getExprSize (BvorExpr a b) = getExprSize a

getExprSize (BvrolExpr a b) = getExprSize a

getExprSize (BvrorExpr a b) = getExprSize a

getExprSize (BvsubExpr a b) = getExprSize a

getExprSize (BvxnorExpr a b) = getExprSize a

getExprSize (BvxorExpr a b) = getExprSize a

getExprSize (BvExpr a) = bvlength a

getExprSize (ConcatExpr a) = sum $ map getExprSize a

getExprSize (EqualExpr a b) = getExprSize a

getExprSize (ExtractExpr l h e) = h - l

getExprSize (ReplaceExpr l a b) = getExprSize a

getExprSize (IteExpr a b c) = getExprSize b

getExprSize (LandExpr a b) = getExprSize a

getExprSize (LnotExpr a) = getExprSize a

getExprSize (LorExpr a b) = getExprSize a

getExprSize (ReferenceExpr a b) = a

getExprSize (SxExpr a b) = a + getExprSize b

getExprSize (ZxExpr a b) = a + getExprSize b

getExprSize (UndefinedExpr a) = a

getExprSize (Load a b) = a * byte_size_bit

getExprSize (GetReg a) = getRegisterSize a

