module EvalAst where

import Ast
import Hapstone.Internal.X86 as X86
import Util
import Data.List
import Data.Bits
import Data.Maybe
import Hapstone.Internal.Capstone as Capstone
import BitVector

-- The RegisterFile is a map from registers to values

type NumRegisterFile = [(CompoundReg, BitVector)]

-- Gets all the register ranges in the RegisterFile

ranges :: NumRegisterFile -> [CompoundReg]

ranges = fst . unzip

-- Determines if the given register has a definite value in the register file

isRegisterDefined :: NumRegisterFile -> CompoundReg -> Bool

isRegisterDefined regFile reg = or (map (isSubregisterOf reg) (ranges regFile))

getRegisterParent :: NumRegisterFile -> CompoundReg -> Maybe CompoundReg

getRegisterParent regFile reg = find (isSubregisterOf reg . fst) regFile >>= Just . fst

-- Gets the value of the specified compound register from the register file

getRegisterValue :: NumRegisterFile -> CompoundReg -> Maybe BitVector

getRegisterValue regFile reg =
  case getRegisterParent regFile reg of
    Just parentReg ->
      let (l, h) = registerSub reg parentReg
      in Just $ bvextract l h $ fromJust $ lookup parentReg regFile
    Nothing -> Nothing

-- Updates the given register file by putting the given value in the given register

updateRegisterFile :: NumRegisterFile -> CompoundReg -> BitVector -> NumRegisterFile

updateRegisterFile regFile reg val =
  let (ranges, _) = unzip regFile
      new_ranges = addRegister ranges reg
      undefRegFile = map (\x -> (x, toBv 0 (getRegisterSize x))) new_ranges
      put regFile (reg, value) = map (\(x, y) ->
        (x, if isSubregisterOf reg x then (let pos = fst (registerSub reg x) in bvreplace y pos value) else y)) regFile
  in foldl put undefRegFile (regFile ++ [(reg, val)])

-- Get the register values from the register file

getRegisterValues :: NumRegisterFile -> [(X86.X86Reg, BitVector)]

getRegisterValues regFile =
  map (\(x, y) -> (x, fromJust $ getRegisterValue regFile y)) (filter (isRegisterDefined regFile . snd) x86RegisterMap)

-- Looks up the given BitVector in the association list

lookupBv :: BitVector -> [(BitVector, BitVector)] -> Maybe BitVector

lookupBv _ [] = Nothing

lookupBv a ((b,c):d) | a == b = Just c

lookupBv a (_:d) = lookupBv a d

-- Gets the specified bytes from memory

getMemoryValue :: [(BitVector, BitVector)] -> BitVector -> Int -> Maybe BitVector

getMemoryValue _ _ 0 = Just empty

getMemoryValue mem a exprSize =
  let nextAddr = a + toBv 1 (bvlength a)
      nextSize = exprSize - byteSizeBit
  in case (lookupBv a mem, getMemoryValue mem nextAddr nextSize) of
    (Just x, Just y) -> Just (bvconcat y x)
    _ -> Nothing

-- Updates the bytes of memory starting at the given address

updateMemory :: [(BitVector, BitVector)] -> BitVector -> BitVector -> [(BitVector, BitVector)]

updateMemory mem _ v | bvlength v == 0 = mem

updateMemory mem d v =
  let nextBv = bvextract byteSizeBit (bvlength v) v
      nextAddr = d + toBv 1 (bvlength d)
      currentVal = bvextract 0 byteSizeBit v
  in updateMemory (assign mem (d, currentVal)) nextAddr nextBv

-- Represents the state of a processor: register file contents, data memory contents, and
-- the processor modes.

data NumExecutionContext = NumExecutionContext {
  -- Holds the contents and validity of the processor registers
  registerFile :: NumRegisterFile,
  -- Holds the contents and validity of the processor memory
  memory :: [(BitVector, BitVector)],
  -- Holds the processor information that effects interpretation of instructions
  procModes :: [CsMode]
} deriving (Eq, Show)

-- Creates a context where memory and the register file are empty

numExecContext :: [CsMode] -> NumExecutionContext

numExecContext modes = NumExecutionContext {
  memory = [],
  registerFile = [],
  procModes = modes
}

-- Make the instruction pointer of the given execution context point to the first
-- instruction

zeroInsnPtr :: NumExecutionContext -> NumExecutionContext

zeroInsnPtr cin =
  cin { registerFile = updateRegisterFile (registerFile cin)
    (getInsnPtr modes) (toBv 0 (getArchBitSize modes)) }
      where modes = procModes cin

-- Set the stack pointer to zero.

zeroStackReg :: NumExecutionContext -> NumExecutionContext

zeroStackReg cin =
  cin { registerFile = updateRegisterFile (registerFile cin)
    (getStackReg modes) (toBv 0 (getArchBitSize modes)) }
      where modes = procModes cin

-- Evaluates the given expression in the given context and returns the result

eval :: NumExecutionContext -> Expr -> BitVector

eval cin (BvExpr a) = a

eval cin (BvmulExpr a b) = eval cin a * eval cin b

eval cin (BvudivExpr a b) = bvudiv (eval cin a) (eval cin b)

eval cin (BvsdivExpr a b) = bvsdiv (eval cin a) (eval cin b)

eval cin (BvxorExpr a b) = bvxor (eval cin a) (eval cin b)

eval cin (BvandExpr a b) = bvand (eval cin a) (eval cin b)

eval cin (BvorExpr a b) = bvor (eval cin a) (eval cin b)

eval cin (BvnotExpr a) = bvnot (eval cin a)

eval cin (BvaddExpr a b) = eval cin a + eval cin b

eval cin (BvsubExpr a b) = eval cin a - eval cin b

eval cin (BvlshrExpr a b) = bvlshr (eval cin a) (eval cin b)

eval cin (ZxExpr a b) = zx a (eval cin b)

eval cin (IteExpr a b c) = if eval cin a == bvtrue then eval cin c else eval cin b

eval cin (ReplaceExpr b c d) = bvreplace (eval cin c) b (eval cin d)

eval cin (ExtractExpr a b c) = bvextract a b (eval cin c)

eval cin (EqualExpr a b) = boolToBv (eval cin a == eval cin b)

eval cin (BvugtExpr a b) = boolToBv $ bvugt (eval cin a) (eval cin b)

eval cin (BvultExpr a b) = boolToBv $ bvult (eval cin a) (eval cin b)

eval cin (BvugeExpr a b) = boolToBv $ bvuge (eval cin a) (eval cin b)

eval cin (BvuleExpr a b) = boolToBv $ bvule (eval cin a) (eval cin b)

eval cin (BvsgtExpr a b) = boolToBv $ bvsgt (eval cin a) (eval cin b)

eval cin (BvsltExpr a b) = boolToBv $ bvslt (eval cin a) (eval cin b)

eval cin (BvsgeExpr a b) = boolToBv $ bvsge (eval cin a) (eval cin b)

eval cin (BvsleExpr a b) = boolToBv $ bvsle (eval cin a) (eval cin b)

eval cin (GetReg bs) =
  case getRegisterValue (registerFile cin) bs of
    Nothing -> error "Read attempted on uninitialized register."
    Just x -> x

eval cin (Load a b) =
  case getMemoryValue (memory cin) (eval cin b) a of
    Nothing -> error "Read attempted on uninitialized memory."
    Just x -> x

-- Assigns the given value to the given key. Adds a new association to the list if necessary

assign :: Eq a => [(a,b)] -> (a, b) -> [(a, b)]

assign [] (a, b) = [(a, b)]

assign ((c, d) : es) (a, b) | c == a = (a, b) : es

assign ((c, d) : es) (a, b) | c /= a = (c, d) : assign es (a, b)

-- Executes the given statement in the given context and returns the resulting context

exec :: NumExecutionContext -> IdStmt -> NumExecutionContext
-- Executes a SetReg operation by setting each byte of the register separately
exec cin (SetReg _ bs a) = cin { registerFile = updateRegisterFile (registerFile cin) bs (eval cin a) }
-- Executes a Store operation by setting each byte of memory separately
exec cin (Store _ dst val) = cin { memory = updateMemory (memory cin) (eval cin dst) (eval cin val) }
-- Executes a Compound statement by executing its constituents in order
exec cin (Compound _ stmts) = foldl exec cin stmts
-- Executes a Comment by just leaving the machine state unchanged
exec cin (Comment _ _) = cin

-- Lookup the statement with the given id

lookupStmt :: [IdStmt] -> Int -> Maybe IdStmt

lookupStmt [] _ = Nothing

lookupStmt (SetReg v bs a : stmts) id | v == id = Just $ SetReg v bs a

lookupStmt (Store v bs a : stmts) id | v == id = Just $ Store v bs a

lookupStmt (Compound v a : stmts) id | v == id = Just $ Compound v a

lookupStmt (Comment v a : stmts) id | v == id = Just $ Comment v a

lookupStmt (stmt : stmts) id = lookupStmt stmts id

-- Executes a group of statements pointed to by the instruction pointer and returns the
-- new context

fetchExec :: NumExecutionContext -> [IdStmt] -> NumExecutionContext

fetchExec cin stmts =
  let procInsnPtr = getInsnPtr (procModes cin)
  in case getRegisterValue (registerFile cin) procInsnPtr of
    Nothing -> error "Instruction pointer has not yet been set."
    Just registerValue ->
      case lookupStmt stmts (fromBvU registerValue) of
        Nothing -> cin
        Just stmt -> fetchExec (exec cin stmt) stmts

