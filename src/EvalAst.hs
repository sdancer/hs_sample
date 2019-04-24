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

-- An empty register file for convenience

emptyRegisterFile :: NumRegisterFile

emptyRegisterFile = []

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

updateRegisterFile reg_file reg val =
  let (ranges, _) = unzip reg_file
      new_ranges = addRegister ranges reg
      undef_reg_file = map (\x -> (x, toBv 0 (getRegisterSize x))) new_ranges
      put reg_file (reg, value) = map (\(x, y) ->
        (x, if isSubregisterOf reg x then (let pos = fst (registerSub reg x) in bvreplace y pos value) else y)) reg_file
  in foldl put undef_reg_file (reg_file ++ [(reg, val)])

-- Get the register values from the register file

getRegisterValues :: NumRegisterFile -> [(X86.X86Reg, BitVector)]

getRegisterValues regFile =
  map (\(x, y) -> (x, fromJust $ getRegisterValue regFile y)) (filter (isRegisterDefined regFile . snd) x86RegisterMap)

-- Gets the specified bytes from memory

getMemoryValue :: [(Int, Int)] -> [Int] -> Maybe BitVector

getMemoryValue _ [] = Just empty

getMemoryValue mem (b:bs) =
  case (lookup b mem, getMemoryValue mem bs) of
    (Just x, Just y) -> Just (bvconcat y (toBv x byte_size_bit))
    _ -> Nothing

-- Represents the state of a processor: register file contents, data memory contents, and
-- the instruction memory.

data NumExecutionContext = NumExecutionContext {
  -- Holds the contents and validity of the processor registers
  reg_file :: NumRegisterFile,
  -- Holds the contents and validity of the processor memory
  memory :: [(Int, Int)],
  -- Holds the processor information that effects interpretation of instructions
  proc_modes :: [CsMode]
} deriving (Eq, Show)

-- Creates a context where the instruction pointer points to the first instruction, and
-- memory and the register file are empty.

basicX86Context :: [CsMode] -> NumExecutionContext

basicX86Context modes = NumExecutionContext {
  memory = [],
  reg_file = updateRegisterFile emptyRegisterFile (get_insn_ptr modes) (toBv 0 (get_arch_bit_size modes)),
  proc_modes = modes
}

-- Evaluates the given expression in the given context and returns the result

eval :: NumExecutionContext -> Expr -> BitVector

eval cin (BvExpr a) = a

eval cin (BvmulExpr a b) = bvmul (eval cin a) (eval cin b)

eval cin (BvudivExpr a b) = bvudiv (eval cin a) (eval cin b)

eval cin (BvsdivExpr a b) = bvsdiv (eval cin a) (eval cin b)

eval cin (BvxorExpr a b) = bvxor (eval cin a) (eval cin b)

eval cin (BvandExpr a b) = bvand (eval cin a) (eval cin b)

eval cin (BvorExpr a b) = bvor (eval cin a) (eval cin b)

eval cin (BvnotExpr a) = bvnot (eval cin a)

eval cin (BvaddExpr a b) = bvadd (eval cin a) (eval cin b)

eval cin (BvsubExpr a b) = bvsub (eval cin a) (eval cin b)

eval cin (BvlshrExpr a b) = bvlshr (eval cin a) (eval cin b)

eval cin (ZxExpr a b) = zx a (eval cin b)

eval cin (IteExpr a b c) = if bvequal (eval cin a) bvtrue then eval cin c else eval cin b

eval cin (ReplaceExpr b c d) = bvreplace (eval cin c) b (eval cin d)

eval cin (ExtractExpr a b c) = bvextract a b (eval cin c)

eval cin (EqualExpr a b) = boolToBv $ bvequal (eval cin a) (eval cin b)

eval cin (BvugtExpr a b) = boolToBv $ bvugt (eval cin a) (eval cin b)

eval cin (BvultExpr a b) = boolToBv $ bvult (eval cin a) (eval cin b)

eval cin (BvugeExpr a b) = boolToBv $ bvuge (eval cin a) (eval cin b)

eval cin (BvuleExpr a b) = boolToBv $ bvule (eval cin a) (eval cin b)

eval cin (BvsgtExpr a b) = boolToBv $ bvsgt (eval cin a) (eval cin b)

eval cin (BvsltExpr a b) = boolToBv $ bvslt (eval cin a) (eval cin b)

eval cin (BvsgeExpr a b) = boolToBv $ bvsge (eval cin a) (eval cin b)

eval cin (BvsleExpr a b) = boolToBv $ bvsle (eval cin a) (eval cin b)

eval cin (GetReg bs) =
  case getRegisterValue (reg_file cin) bs of
    Nothing -> error "Read attempted on uninitialized memory."
    Just x -> x

eval cin (Load a b) =
  let memStart = fromBvU (eval cin b)
      memVal = getMemoryValue (memory cin) [memStart..(memStart + (div a byte_size_bit) - 1)]
  in case memVal of
    Nothing -> error "Read attempted on uninitialized memory."
    Just x -> x

-- Assigns the given value to the given key. Adds a new association to the list if necessary

assign :: Eq a => [(a,b)] -> (a, b) -> [(a, b)]

assign [] (a, b) = [(a, b)]

assign ((c, d) : es) (a, b) | c == a = (a, b) : es

assign ((c, d) : es) (a, b) | c /= a = (c, d) : assign es (a, b)

exec :: NumExecutionContext -> Stmt a -> NumExecutionContext

-- Executes a SetReg operation by setting each byte of the register separately

exec cin (SetReg _ bs a) =
  cin { reg_file = updateRegisterFile (reg_file cin) bs (eval cin a) }

-- Executes a Store operation by setting each byte of memory separately

exec cin (Store _ dst val) =
  let updateMemory mem 0 _ _ = mem
      updateMemory mem c d v =
        updateMemory (assign mem (d, (v .&. (bit byte_size_bit - 1)))) (c - 1) (d + 1) (shift v (-byte_size_bit))
  in cin { memory = updateMemory (memory cin) (getExprSize val) (fromBvU (eval cin dst)) (fromBvU (eval cin val)) }

-- Executes a Compound statement by executing its constituents in order

exec cin (Compound _ stmts) = foldl exec cin stmts

-- Lookup the statement with the given id

lookupStmt :: Eq a => [Stmt (Maybe a)] -> a -> Stmt (Maybe a)

lookupStmt (SetReg (Just v) bs a : stmts) id | v == id = SetReg (Just v) bs a

lookupStmt (Store (Just v) bs a : stmts) id | v == id = Store (Just v) bs a

lookupStmt (Compound (Just v) a : stmts) id | v == id = Compound (Just v) a

lookupStmt (stmt : stmts) id = lookupStmt stmts id

-- Executes a group of statements pointed to by the instruction pointer and returns the
-- new context

step :: [Stmt (Maybe Int)] -> NumExecutionContext -> NumExecutionContext

step stmts cin =
  let procInsnPtr = get_insn_ptr (proc_modes cin)
  in case getRegisterValue (reg_file cin) procInsnPtr of
    Nothing -> error "Instruction pointer has not yet been set."
    Just registerValue -> exec cin (lookupStmt stmts (fromBvU registerValue))

-- Applies the given function on the given argument a given number of times

iter :: (a -> a) -> Int -> a -> a

iter fun 0 x = x

iter fun n x = iter fun (n - 1) (fun x)

