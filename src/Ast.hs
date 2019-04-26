module Ast where

import Data.Word
import Data.List
import Hapstone.Internal.X86 as X86
import Hapstone.Internal.Capstone as Capstone
import Data.Bits
import Util
import BitVector
import Data.SBV
import Data.SBV.Dynamic
import Data.Maybe
import Control.Monad.State.Lazy
import Data.Coerce
import Data.Functor.Identity
import Debug.Trace

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
reg_file_bits = 192 * byte_size_bit

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

get_arch_bit_size = (* byte_size_bit) . get_arch_byte_size

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

-- Checks if the given register is contained within the list

isRegisterContained :: [CompoundReg] -> CompoundReg -> Bool

isRegisterContained regs reg = or $ map (isSubregisterOf reg) regs

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
-- context.

data Expr =
  -- Literal bit-vector. The size of this expression equals that of the bit-vector.
  BvExpr BitVector
  
  -- The following operations take and return equally sized operands
  | BvaddExpr Expr Expr -- Add
  | BvandExpr Expr Expr -- Bitwise and
  | BvashrExpr Expr Expr -- Arithmetic shift right
  | BvlshrExpr Expr Expr -- Logical shift right
  | BvmulExpr Expr Expr -- Multiply
  | BvnegExpr Expr -- Negate
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
  | BvxorExpr Expr Expr -- Bitwise xor
  | EqualExpr Expr Expr -- Returns 1 is equal, 0 otherwise
  | IteExpr Expr Expr Expr -- If first expression is non-zero then return second, otherwise third
  
  -- Takes in order the low bit index (inclusive), the high bit index (exclusive), and the
  -- expression from which to extract. Size of resulting expression is the difference
  -- between the indicies.
  | ExtractExpr Int Int Expr
  -- Takes in order the low bit index (inclusive), the expression whose part starting at
  -- the aforementioned bit is to be replaced, and the expression that is to be used as
  -- the replacement. Size of returned expression equals that of first supplied expression.
  | ReplaceExpr Int Expr Expr
  -- Takes in order the size of the expression that is being referenced, and the
  -- identifier of the expression being referenced. Size of this expression equals the
  -- size of the expression being referenced.
  | ReferenceExpr Int Int
  -- Sign extends the given expression to the given size. Size of this expression equals
  -- the given amount.
  | SxExpr Int Expr
  -- Zero extends the given expression to the given size. Size of this expression equals
  -- the given amount.
  | ZxExpr Int Expr
  -- An undefined expression of the given size.
  | UndefinedExpr Int
  -- Takes in order the number of bits to extract from memory, and the byte address in
  -- memory from which to obtain data. Size of this expression equals the number of bits
  -- to be extracted from memory.
  | Load Int Expr
  -- Obtains the value of the given register. The size of this expression equals the size,
  -- in bits, of the register.
  | GetReg CompoundReg
  
  | ConcatExpr [Expr]
  | DecimalExpr Int --float?
  | DeclareExpr --wtf?
  | DistinctExpr Expr Expr
  | IffExpr Expr Expr -- ! `(iff <expr1> <expr2>)`
  | LetExpr String Expr Expr
  | StringExpr String
  | VariableExpr
  deriving (Eq, Show)

-- The statements that the machine code will be lifted to. Statements modify context.

data Stmt a b c d =
  -- Takes in order the id of the statement, the address at which to put the expression,
  -- and the expression to store.
    Store a Expr Expr
  -- Takes in order the id of the statement, the register in which to put the expression,
  -- and the expression to store. The size of the expression must equal the size of the
  -- register.
  | SetReg b CompoundReg Expr
  -- Takes in order the id of the statement, and an ordered list of statements that will
  -- be executed when this statement is executed.
  | Compound c [Stmt a b c d]
  -- An inert statement where the String argument is the actual comment. Some conditions
  -- that may cause the generation of this constructor are invalid object code and
  -- usage of unsupported instructions. Comment statements are used to ensure the
  -- successful code analysis even in the presence of hostile program inputs.
  | Comment d String
  deriving (Eq, Show)

-- A statement with a id annotations

type IdStmt = Stmt Int Int Int Int

-- A statement annotated with ids and absolute expressions. Store is parameterized by an
-- id, an absolute memory expression, and an absolute value expression. SetReg is
-- parameterized by an id, and an absolute value expression. The rest are just
-- parameterized by ids.

type AbsStmt = Stmt (Int, Expr, Expr) (Int, Expr) Int Int

-- The size of an expression can be determined statically directly from it and its
-- subexpressions.

getExprSize :: Expr -> Int

getExprSize (BvmulExpr a b) = getExprSize a

getExprSize (BvudivExpr a b) = getExprSize a

getExprSize (BvsdivExpr a b) = getExprSize a

getExprSize (BvugtExpr a b) = 1

getExprSize (BvultExpr a b) = 1

getExprSize (BvugeExpr a b) = 1

getExprSize (BvuleExpr a b) = 1

getExprSize (BvsgtExpr a b) = 1

getExprSize (BvsltExpr a b) = 1

getExprSize (BvsgeExpr a b) = 1

getExprSize (BvsleExpr a b) = 1

getExprSize (BvaddExpr a b) = getExprSize a

getExprSize (BvandExpr a b) = getExprSize a

getExprSize (BvashrExpr a b) = getExprSize a

getExprSize (BvlshrExpr a b) = getExprSize a

getExprSize (BvnegExpr a) = getExprSize a

getExprSize (BvnotExpr a) = getExprSize a

getExprSize (BvorExpr a b) = getExprSize a

getExprSize (BvrolExpr a b) = getExprSize a

getExprSize (BvrorExpr a b) = getExprSize a

getExprSize (BvsubExpr a b) = getExprSize a

getExprSize (BvxorExpr a b) = getExprSize a

getExprSize (BvExpr a) = bvlength a

getExprSize (ConcatExpr a) = sum $ map getExprSize a

getExprSize (EqualExpr a b) = 1

getExprSize (ExtractExpr l h e) = h - l

getExprSize (ReplaceExpr l a b) = getExprSize a

getExprSize (IteExpr a b c) = getExprSize b

getExprSize (ReferenceExpr a b) = a

getExprSize (SxExpr a b) = a

getExprSize (ZxExpr a b) = a

getExprSize (UndefinedExpr a) = a

getExprSize (Load a b) = a

getExprSize (GetReg a) = getRegisterSize a

-- Creates a post-ordered list of expressions from the given expression. Sometimes the
-- hierarchy is a hindrance in computations.

flatten :: Expr -> [Expr]

flatten (BvExpr bv) = [(BvExpr bv)]

flatten (BvmulExpr a b) = flatten a ++ flatten b ++ [BvmulExpr a b]

flatten (BvudivExpr a b) = flatten a ++ flatten b ++ [BvudivExpr a b]

flatten (BvsdivExpr a b) = flatten a ++ flatten b ++ [BvsdivExpr a b]

flatten (BvugtExpr a b) = flatten a ++ flatten b ++ [BvugtExpr a b]

flatten (BvultExpr a b) = flatten a ++ flatten b ++ [BvultExpr a b]

flatten (BvugeExpr a b) = flatten a ++ flatten b ++ [BvugeExpr a b]

flatten (BvuleExpr a b) = flatten a ++ flatten b ++ [BvuleExpr a b]

flatten (BvsgtExpr a b) = flatten a ++ flatten b ++ [BvsgtExpr a b]

flatten (BvsltExpr a b) = flatten a ++ flatten b ++ [BvsltExpr a b]

flatten (BvsgeExpr a b) = flatten a ++ flatten b ++ [BvsgeExpr a b]

flatten (BvsleExpr a b) = flatten a ++ flatten b ++ [BvsleExpr a b]

flatten (BvaddExpr a b) = flatten a ++ flatten b ++ [BvaddExpr a b]

flatten (BvandExpr a b) = flatten a ++ flatten b ++ [BvandExpr a b]

flatten (BvashrExpr a b) = flatten a ++ flatten b ++ [BvashrExpr a b]

flatten (BvlshrExpr a b) = flatten a ++ flatten b ++ [BvlshrExpr a b]

flatten (BvnegExpr a) = flatten a ++ [BvnegExpr a]

flatten (BvnotExpr a) = flatten a ++ [BvnotExpr a]

flatten (BvorExpr a b) = flatten a ++ flatten b ++ [BvorExpr a b]

flatten (BvrolExpr a b) = flatten a ++ flatten b ++ [BvrolExpr a b]

flatten (BvrorExpr a b) = flatten a ++ flatten b ++ [BvrorExpr a b]

flatten (BvsubExpr a b) = flatten a ++ flatten b ++ [BvsubExpr a b]

flatten (BvxorExpr a b) = flatten a ++ flatten b ++ [BvxorExpr a b]

flatten (ConcatExpr a) = foldl (++) [] (map flatten a) ++ [ConcatExpr a]

flatten (EqualExpr a b) = flatten a ++ flatten b ++ [EqualExpr a b]

flatten (ExtractExpr l h e) = flatten e ++ [ExtractExpr l h e]

flatten (ReplaceExpr l a b) = flatten a ++ flatten b ++ [ReplaceExpr l a b]

flatten (IteExpr a b c) = flatten a ++ flatten b ++ flatten c ++ [IteExpr a b c]

flatten (ReferenceExpr a b) = [ReferenceExpr a b]

flatten (SxExpr a b) = flatten b ++ [SxExpr a b]

flatten (ZxExpr a b) = flatten b ++ [ZxExpr a b]

flatten (UndefinedExpr a) = [UndefinedExpr a]
-- Does not recur into b. This is in order to be consistent with mapMExpr and exprToSVal
-- which also do not recur into b.
flatten (Load a b) = [Load a b]

flatten (GetReg a) = [GetReg a]

-- Going through the expression tree in post-order, monadically map expressions to other
-- expressions using the given function.

mapMExpr :: Monad m => (Expr -> m Expr) -> Expr -> m Expr

mapMExpr f (BvExpr bv) = f (BvExpr bv)

mapMExpr f (BvmulExpr a b) = do { ma <- mapMExpr f a; mb <- mapMExpr f b; f $ BvmulExpr ma mb; }

mapMExpr f (BvudivExpr a b) = do { ma <- mapMExpr f a; mb <- mapMExpr f b; f $ BvudivExpr ma mb; }

mapMExpr f (BvsdivExpr a b) = do { ma <- mapMExpr f a; mb <- mapMExpr f b; f $ BvsdivExpr ma mb; }

mapMExpr f (BvugtExpr a b) = do { ma <- mapMExpr f a; mb <- mapMExpr f b; f $ BvugtExpr ma mb; }

mapMExpr f (BvultExpr a b) = do { ma <- mapMExpr f a; mb <- mapMExpr f b; f $ BvultExpr ma mb; }

mapMExpr f (BvugeExpr a b) = do { ma <- mapMExpr f a; mb <- mapMExpr f b; f $ BvugeExpr ma mb; }

mapMExpr f (BvuleExpr a b) = do { ma <- mapMExpr f a; mb <- mapMExpr f b; f $ BvuleExpr ma mb; }

mapMExpr f (BvsgtExpr a b) = do { ma <- mapMExpr f a; mb <- mapMExpr f b; f $ BvsgtExpr ma mb; }

mapMExpr f (BvsltExpr a b) = do { ma <- mapMExpr f a; mb <- mapMExpr f b; f $ BvsltExpr ma mb; }

mapMExpr f (BvsgeExpr a b) = do { ma <- mapMExpr f a; mb <- mapMExpr f b; f $ BvsgeExpr ma mb; }

mapMExpr f (BvsleExpr a b) = do { ma <- mapMExpr f a; mb <- mapMExpr f b; f $ BvsleExpr ma mb; }

mapMExpr f (BvaddExpr a b) = do { ma <- mapMExpr f a; mb <- mapMExpr f b; f $ BvaddExpr ma mb; }

mapMExpr f (BvandExpr a b) = do { ma <- mapMExpr f a; mb <- mapMExpr f b; f $ BvandExpr ma mb; }

mapMExpr f (BvashrExpr a b) = do { ma <- mapMExpr f a; mb <- mapMExpr f b; f $ BvashrExpr ma mb; }

mapMExpr f (BvlshrExpr a b) = do { ma <- mapMExpr f a; mb <- mapMExpr f b; f $ BvlshrExpr ma mb; }

mapMExpr f (BvnegExpr a) = do { ma <- mapMExpr f a; f $ BvnegExpr ma; }

mapMExpr f (BvnotExpr a) = do { ma <- mapMExpr f a; f $ BvnotExpr ma; }

mapMExpr f (BvorExpr a b) = do { ma <- mapMExpr f a; mb <- mapMExpr f b; f $ BvorExpr ma mb; }

mapMExpr f (BvrolExpr a b) = do { ma <- mapMExpr f a; mb <- mapMExpr f b; f $ BvrolExpr ma mb; }

mapMExpr f (BvrorExpr a b) = do { ma <- mapMExpr f a; mb <- mapMExpr f b; f $ BvrorExpr ma mb; }

mapMExpr f (BvsubExpr a b) = do { ma <- mapMExpr f a; mb <- mapMExpr f b; f $ BvsubExpr ma mb; }

mapMExpr f (BvxorExpr a b) = do { ma <- mapMExpr f a; mb <- mapMExpr f b; f $ BvxorExpr ma mb; }

mapMExpr f (ConcatExpr a) = do { ml <- mapM (mapMExpr f) a; f $ ConcatExpr ml; }

mapMExpr f (EqualExpr a b) = do { ma <- mapMExpr f a; mb <- mapMExpr f b; f $ EqualExpr ma mb; }

mapMExpr f (ExtractExpr l h e) = do { me <- mapMExpr f e; f $ ExtractExpr l h me; }

mapMExpr f (ReplaceExpr l a b) = do { ma <- mapMExpr f a; mb <- mapMExpr f b; f $ ReplaceExpr l ma mb; }

mapMExpr f (IteExpr a b c) = do { ma <- mapMExpr f a; mb <- mapMExpr f b; mc <- mapMExpr f c; f $ IteExpr ma mb mc; }

mapMExpr f (ReferenceExpr a b) = f $ ReferenceExpr a b

mapMExpr f (SxExpr a b) = do { mb <- mapMExpr f b; f $ SxExpr a mb; }

mapMExpr f (ZxExpr a b) = do { mb <- mapMExpr f b; f $ ZxExpr a mb; }

mapMExpr f (UndefinedExpr a) = f $ UndefinedExpr a
-- Must not recur into b as this will interfere with the symbolic evaluator's addressing
-- logic.
mapMExpr f (Load a b) = f $ Load a b

mapMExpr f (GetReg a) = f $ GetReg a

-- Going through the expression tree in post-order, map expressions to other expressions
-- using the given function.

mapExpr :: (Expr -> Expr) -> Expr -> Expr

mapExpr f e = runIdentity $ mapMExpr (return . f) e

boolToBvSVal :: SVal -> SVal

boolToBvSVal e = svIte e (svInteger (KBounded False 1) 1) (svInteger (KBounded False 1) 0)

bvToBool :: SVal -> SVal

bvToBool e = svIte (svEqual e (svInteger (KBounded False 1) 1)) svTrue svFalse

-- Converts an Expr to an m SVal where MonadSymbolic m. This is to enable SMT solvers to
-- prove various things about the expressions synthesized from program binaries.

exprToSVal :: MonadSymbolic m => Expr -> (StateT [(Expr, SVal)] m) SVal

exprToSVal (BvExpr a) = return $ svInteger (KBounded False (bvlength a)) (fromBvU a)

exprToSVal (BvaddExpr a b) = do { sva <- exprToSVal a; svb <- exprToSVal b; return $ svPlus sva svb; }

exprToSVal (BvandExpr a b) = do { sva <- exprToSVal a; svb <- exprToSVal b; return $ svAnd sva svb; }

exprToSVal (BvashrExpr a b) = do { sva <- exprToSVal a; svb <- exprToSVal b; return $ svShiftRight (svSign sva) svb; }

exprToSVal (BvlshrExpr a b) = do { sva <- exprToSVal a; svb <- exprToSVal b; return $ svShiftRight (svUnsign sva) svb; }

exprToSVal (BvmulExpr a b) = do { sva <- exprToSVal a; svb <- exprToSVal b; return $ svTimes sva svb; }

exprToSVal (BvnegExpr a) = do { sva <- exprToSVal a; return $ svUNeg sva; }

exprToSVal (BvnotExpr a) = do { sva <- exprToSVal a; return $ svNot sva; }

exprToSVal (BvorExpr a b) = do { sva <- exprToSVal a; svb <- exprToSVal b; return $ svOr sva svb; }

exprToSVal (BvrolExpr a b) = do { sva <- exprToSVal a; svb <- exprToSVal b; return $ svRotateLeft sva svb; }

exprToSVal (BvrorExpr a b) = do { sva <- exprToSVal a; svb <- exprToSVal b; return $ svRotateRight sva svb; }

exprToSVal (BvsdivExpr a b) = do { sva <- exprToSVal a; svb <- exprToSVal b; return $ svQuot (svSign sva) (svSign svb); }

exprToSVal (BvshlExpr a b) = do { sva <- exprToSVal a; svb <- exprToSVal b; return $ svShiftLeft sva svb; }

exprToSVal (BvsremExpr a b) = do { sva <- exprToSVal a; svb <- exprToSVal b; return $ svRem (svSign sva) (svSign svb); }

exprToSVal (BvsubExpr a b) = do { sva <- exprToSVal a; svb <- exprToSVal b; return $ svMinus sva svb; }

exprToSVal (BvudivExpr a b) = do { sva <- exprToSVal a; svb <- exprToSVal b; return $ svQuot (svUnsign sva) (svUnsign svb); }

exprToSVal (BvuremExpr a b) = do { sva <- exprToSVal a; svb <- exprToSVal b; return $ svRem (svUnsign sva) (svUnsign svb); }

exprToSVal (BvxorExpr a b) = do { sva <- exprToSVal a; svb <- exprToSVal b; return $ svXOr sva svb; }

exprToSVal (EqualExpr a b) = do { sva <- exprToSVal a; svb <- exprToSVal b; return $ boolToBvSVal $ svEqual sva svb; }

exprToSVal (IteExpr a b c) = do { sva <- exprToSVal a; svb <- exprToSVal b; svc <- exprToSVal c; return $ svIte (bvToBool sva) svb svc; }

exprToSVal (ExtractExpr a b c) = do { svc <- exprToSVal c; return $ svExtract (b-1) a svc; }

exprToSVal (BvsgeExpr a b) = do
  sva <- exprToSVal a
  svb <- exprToSVal b
  return $ boolToBvSVal $ svGreaterEq (svSign sva) (svSign svb)

exprToSVal (BvsgtExpr a b) = do
  sva <- exprToSVal a
  svb <- exprToSVal b
  return $ boolToBvSVal $ svGreaterThan (svSign sva) (svSign svb)

exprToSVal (BvsleExpr a b) = do
  sva <- exprToSVal a
  svb <- exprToSVal b
  return $ boolToBvSVal $ svLessEq (svSign sva) (svSign svb)

exprToSVal (BvsltExpr a b) = do
  sva <- exprToSVal a
  svb <- exprToSVal b
  return $ boolToBvSVal $ svLessThan (svSign sva) (svSign svb)

exprToSVal (BvugeExpr a b) = do
  sva <- exprToSVal a
  svb <- exprToSVal b
  return $ boolToBvSVal $ svGreaterEq (svUnsign sva) (svUnsign svb)

exprToSVal (BvugtExpr a b) = do
  sva <- exprToSVal a
  svb <- exprToSVal b
  return $ boolToBvSVal $ svGreaterThan (svUnsign sva) (svUnsign svb)

exprToSVal (BvuleExpr a b) = do
  sva <- exprToSVal a
  svb <- exprToSVal b
  return $ boolToBvSVal $ svLessEq (svUnsign sva) (svUnsign svb)

exprToSVal (BvultExpr a b) = do
  sva <- exprToSVal a
  svb <- exprToSVal b
  return $ boolToBvSVal $ svLessThan (svUnsign sva) (svUnsign svb)

exprToSVal (ReplaceExpr a b c) = do
  svc <- exprToSVal c
  svb <- exprToSVal b
  return $ svJoin (svExtract (getExprSize b - 1) (a + getExprSize c) svb) (svJoin svc (svExtract (a - 1) 0 svb))
-- The semantics for extension expressions is that the given expression is coerced to the
-- given length. If the given length is less than the size of the given expression, then
-- truncation happens.
exprToSVal (ZxExpr a b) = do
  svt <- exprToSVal (BvExpr $ bvzero a)
  svb <- exprToSVal b
  return $ svExtract (a-1) 0 (svJoin svt svb)
-- No SBV function found for sign extension. Hack: concatenate zero vector to the right of
-- the given expression, and then arithmetic shift right.
exprToSVal (SxExpr a b) = do
  svb <- exprToSVal b
  svt <- exprToSVal (BvExpr $ bvzero a)
  return $ svExtract (a - 1) 0 (svShr (svSign $ svJoin svb svt) a)

-- The following lookups are done in order to ensure that references to the same things
-- get the same symbols

-- Turn a ReferenceExpr into an SVal by looking up the corresponding SVal in the current
-- state. If it is not there, create a new symbol and add it to the state.

exprToSVal (ReferenceExpr a b) = do
  exprValAssocs <- get
  case lookup (ReferenceExpr a b) exprValAssocs of
    Nothing -> do
      let newExpr = ReferenceExpr a b
      newVar <- sWordN_ a
      put ((newExpr, newVar) : exprValAssocs)
      exprToSVal (ReferenceExpr a b)
    Just val -> return $ val

-- Turn a Load expression into an SVal by separately converting each of its bytes into
-- symbols and joining these symbols together. Add the new associations between
-- expressions and symbols to the state.

exprToSVal (Load 0 _) = exprToSVal (BvExpr (bvzero 0))

exprToSVal (Load a b) = do
  exprValAssocs <- get
  let (exprs, svals) = unzip exprValAssocs
  results <- mapM (exprEquals (Load byte_size_bit b)) exprs
  if elem Nothing results then
    fail "Could not determine the equality of two expressions."
  else if not $ elem (Just True) results then do
    let newExpr = Load byte_size_bit b
    newVar <- sWordN_ byte_size_bit
    put ((newExpr, newVar) : exprValAssocs)
    exprToSVal (Load a b)
  else do
    let boolValAssocs = zip results svals
        val = fromJust $ lookup (Just True) boolValAssocs
    rest <- exprToSVal (Load (a - byte_size_bit) (BvaddExpr b (BvExpr $ bvone (getExprSize b))))
    return $ svJoin val rest

-- Turn a GetReg into an SVal by looking up the corresponding SVal in the current
-- state. If it is not there, create a new symbol and add it to the state.

exprToSVal (GetReg a) = do
  exprValAssocs <- get
  let rootRegister = getRootRegister a
  case lookup (GetReg rootRegister) exprValAssocs of
    Nothing -> do
      let newExpr = GetReg rootRegister
      newVar <- sWordN_ (getRegisterSize rootRegister)
      put ((newExpr, newVar) : exprValAssocs)
      exprToSVal (GetReg a)
    Just val -> do
      let (l,h) = registerSub a rootRegister
      return $ svExtract (h-1) l val

-- If the expression is undefined, then just make an anonymous variable of the same size.
-- If this variable does actually contribute to the value of the expression, then the
-- SMT solver will never be able to prove the equality between this expression and any
-- other.

exprToSVal (UndefinedExpr a) = sWordN_ a

-- Checks if two expressions are equal using an SMT solver. Returns Just True after I/O if
-- SMT solver is able to prove that expressions are equal for all variable assignments.
-- Returns Just False after I/O if SMT solver is able to prove that expressions are
-- unequal for all variable assignments. Returns Nothing after I/O is SMT solver is not
-- able to prove either.

exprEquals :: MonadIO m => Expr -> Expr -> m (Maybe Bool)

exprEquals a b = liftIO $ do
  -- Try to prove that expressions are equal for all variable assignments
  thmRes <- Data.SBV.Dynamic.proveWith z3 $ evalStateT (do
    sva <- exprToSVal a
    svb <- exprToSVal b
    return $ svEqual sva svb) []
  -- Try to prove that expressions are unequal for all variable assignments
  negThmRes <- Data.SBV.Dynamic.proveWith z3 $ evalStateT (do
    sva <- exprToSVal a
    svb <- exprToSVal b
    return $ svNotEqual sva svb) []
  -- Poke into internals of ThmResult. Not good.
  return $ case coerce (thmRes, negThmRes) of
    (Unsatisfiable _ _, _) -> Just True
    (_, Unsatisfiable _ _) -> Just False
    _ -> Nothing

