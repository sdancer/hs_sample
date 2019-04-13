module SymbolicEval where

import Ast
import Hapstone.Internal.X86 as X86
import Util
import Data.Bits
import Data.List
import Data.Maybe
import Hapstone.Internal.Capstone as Capstone
import BitVector

-- Assigns the given value to the given key. Adds a new association to the list if necessary

assign :: Eq a => [(a,b)] -> (a, b) -> [(a, b)]

assign [] (a, b) = [(a, b)]

assign ((c, d) : es) (a, b) | c == a = (a, b) : es

assign ((c, d) : es) (a, b) | c /= a = (c, d) : assign es (a, b)

-- The RegisterFile is a map from registers to values

type SymRegisterFile = [(CompoundReg, Expr)]

-- Gets all the register ranges in the RegisterFile

ranges :: SymRegisterFile -> [CompoundReg]

ranges = fst . unzip

-- An empty register file for convenience

emptyRegisterFile :: SymRegisterFile

emptyRegisterFile = []

-- Determines if the given register has a definite value in the register file

isRegisterDefined :: SymRegisterFile -> CompoundReg -> Bool

isRegisterDefined regFile reg = or (map (isSubregisterOf reg) (ranges regFile))

-- Extracts the expression in the given bit-range of the given expression

extractExpr :: Int -> Int -> Expr -> Expr

-- The extraction is the entire expression
extractExpr l h e | h - l == getExprSize e = e
-- The extraction is being done on a literal
extractExpr l h (BvExpr a) = BvExpr $ bvextract l h a
-- The extraction is within the replacement expression
extractExpr l h (ReplaceExpr a e f) | a <= l && h <= a + getExprSize f =
  extractExpr y z f where y = l - a; z = h - a
-- The replacement expression is disjoint from the extraction
extractExpr l h (ReplaceExpr a e f) | a + getExprSize f <= l || h <= a = extractExpr l h e
-- No simplification possible
extractExpr l h e = ExtractExpr l h e

-- Gets the value of the specified compound register from the register file

getRegisterValue :: SymRegisterFile -> CompoundReg -> Expr

getRegisterValue regFile reg =
  let rootRegister = getRootRegister reg
      (l,h) = registerSub reg rootRegister
  in case lookup rootRegister regFile of
    Just x -> extractExpr l h x
    Nothing -> GetReg reg

-- Replace the expression in the given bit-range of the given expression

replaceExpr :: Int -> Expr -> Expr -> Expr

-- The entire expression is being replaced
replaceExpr l a b | getExprSize a == getExprSize b = b
-- A part of a literal is being replaced by another literal
replaceExpr l (BvExpr a) (BvExpr b) = BvExpr $ bvreplace a l b
-- The current replacement coincides with a previous replacement
replaceExpr l (ReplaceExpr a b c) e | l == a && getExprSize e == getExprSize c = ReplaceExpr l b e
-- The current replacement is disjoint from previous replacement
replaceExpr l (ReplaceExpr a b c) e | l + getExprSize e <= a || a + getExprSize c <= l =
  ReplaceExpr a (replaceExpr l b e) c
-- The current replacement cannot be simplified
replaceExpr l f e = ReplaceExpr l f e

-- Updates the given register file by putting the given value in the given register

setRegisterValue :: SymRegisterFile -> CompoundReg -> Expr -> SymRegisterFile

setRegisterValue regFile reg val =
  let rootRegister = getRootRegister reg
      (l,h) = registerSub reg rootRegister
      newRootExpr = replaceExpr l (getRegisterValue regFile rootRegister) val
  in assign regFile (rootRegister, newRootExpr)

-- Updates the register file by removing the value in the given register

unsetRegisterValue :: SymRegisterFile -> CompoundReg -> SymRegisterFile

unsetRegisterValue regFile reg = setRegisterValue regFile reg (UndefinedExpr $ getRegisterSize reg)

-- Get the register values from the register file

getRegisterValues :: SymRegisterFile -> [(X86.X86Reg, Expr)]

getRegisterValues regFile =
  map (\(x, y) -> (x, getRegisterValue regFile y)) (filter (isRegisterDefined regFile . snd) x86RegisterMap)

-- Gets the specified bytes from memory

getMemoryValue :: [(Int, Expr)] -> (Int, Int) -> Expr

getMemoryValue mem (a,b) =
  let exprSize = (b-a) * byte_size_bit
      setByte expr offset =
        replaceExpr (offset*byte_size_bit) expr $
          case lookup (a+offset) mem of
            Just x -> x
            _ -> Load 1 (BvExpr $ intToBv (a+offset))
  in foldl setByte (UndefinedExpr exprSize) [0..b-a-1]

-- Represents the state of a processor: register file contents, data memory contents, and
-- the instruction memory.

data SymExecutionContext = SymExecutionContext {
  reg_file :: SymRegisterFile, -- Holds the contents and validity of the processor registers
  memory :: [(Int, Expr)], -- Holds the contents and validity of the processor memory
  proc_modes :: [CsMode] -- Holds the processor information that effects interpretation of instructions
} deriving (Eq, Show)

-- Creates a context where the memory and the register file are empty.

basicX86Context :: [CsMode] -> SymExecutionContext

basicX86Context modes = SymExecutionContext {
  memory = [],
  reg_file = emptyRegisterFile,
  proc_modes = modes
}

-- Simplifies the given expression in the given context

symEval :: SymExecutionContext -> Expr -> Expr

symEval cin (BvxorExpr c d) =
  case (symEval cin c, symEval cin d) of
    (BvExpr abv, BvExpr bbv) -> BvExpr (bvxor abv bbv)
    (c, d) -> BvxorExpr c d

symEval cin (BvandExpr c d) =
  case (symEval cin c, symEval cin d) of
    (BvExpr abv, BvExpr bbv) -> BvExpr (bvand abv bbv)
    (c, d) -> BvandExpr c d

symEval cin (BvorExpr c d) =
  case (symEval cin c, symEval cin d) of
    (BvExpr abv, BvExpr bbv) -> BvExpr (bvor abv bbv)
    (c, d) -> BvorExpr c d

symEval cin (BvnotExpr c) =
  case (symEval cin c) of
    (BvExpr abv) -> BvExpr (bvnot abv)
    (c) -> BvnotExpr c

symEval cin (EqualExpr c d) =
  case (symEval cin c, symEval cin d) of
    (BvExpr abv, BvExpr bbv) -> BvExpr (if equal abv bbv then one abv else zero abv)
    (c, d) -> EqualExpr c d

symEval cin (BvaddExpr c d) =
  case (symEval cin c, symEval cin d) of
    (BvExpr abv, BvExpr bbv) -> BvExpr (bvadd abv bbv)
    (c, d) -> BvaddExpr c d

symEval cin (BvsubExpr c d) =
  case (symEval cin c, symEval cin d) of
    (BvExpr abv, BvExpr bbv) -> BvExpr (bvsub abv bbv)
    (c, d) -> BvsubExpr c d

symEval cin (BvlshrExpr c d) =
  case (symEval cin c, symEval cin d) of
    (BvExpr abv, BvExpr bbv) -> BvExpr (bvlshr abv bbv)
    (c, d) -> BvlshrExpr c d

symEval cin (ZxExpr a c) =
  case (symEval cin c) of
    (BvExpr bbv) -> BvExpr (zx a bbv)
    (c) -> ZxExpr a c

symEval cin (IteExpr a b c) =
  case (symEval cin a, symEval cin b, symEval cin c) of
    (BvExpr a, b, c) -> if equal a (zero a) then c else b
    (a, b, c) -> IteExpr a b c

symEval cin (ReplaceExpr b c d) =
  case (symEval cin c, symEval cin d) of
    (BvExpr cbv, BvExpr dbv) -> BvExpr (bvreplace cbv b dbv)
    (c, d) -> ReplaceExpr b c d

symEval cin (ExtractExpr a b c) =
  case (symEval cin c) of
    (BvExpr d) -> BvExpr (bvextract a b d)
    (c) -> ExtractExpr a b c

symEval cin (GetReg bs) = getRegisterValue (reg_file cin) bs

-- add ro memory (raise exception if written)
-- add symbolic addressed memory (example stack, no concrete values mapping references)

symEval cin (Load a b) =
  case (symEval cin b) of
    (BvExpr memStartBv) ->
      let memStart = bvToInt memStartBv
      in getMemoryValue (memory cin) (memStart,memStart + a)
    (b) -> Load a b

symEval cin expr = expr

-- Put the given expression into memory starting at the given address and return the new
-- context.

updateMemory :: SymExecutionContext -> Int -> Expr -> SymExecutionContext

updateMemory cin address val =
  let bc = div (getExprSize val) byte_size_bit
      updateByte mem x =
        assign mem (address + x, extractExpr (x*byte_size_bit) ((x+1)*byte_size_bit) val)
      nmem = foldl updateByte (memory cin) [0..bc]
  in cin { memory = nmem }

-- Symbolically executes the statement on the given context, potentially simplifying it in
-- the process. Put the result of the simplification or a self-reference into storage.
-- Returns resulting context.

symExec :: SymExecutionContext -> Stmt Int -> (SymExecutionContext, Stmt Int)

symExec cin (SetReg id bs a) =
  let exprVal = symEval cin a
      regVal = case exprVal of
        BvExpr v -> BvExpr v
        _ -> GetReg bs
  in (cin { reg_file = setRegisterValue (reg_file cin) bs regVal }, SetReg id bs exprVal)

symExec cin (Store id dst val) =
  let pdest = symEval cin dst
      pval = symEval cin val
      memVal = case pval of
        BvExpr v -> BvExpr v
        _ -> Load (getExprSize val) dst
  in
      case pdest of
        BvExpr a -> (updateMemory cin (bvToInt a) memVal, Store id pdest pval)
        _ -> error "Store on symbolic mem not implemented"

symExec cin (Compound id stmts) = (i, Compound id s)
  where (i,s) = mapAccumL symExec cin stmts

-- Labels all the statements in the instructions with a new unique identifier

labelStmts :: Int -> Stmt a -> (Int, Stmt Int)

labelStmts start (SetReg id bs a) = (start + 1, SetReg start bs a)

labelStmts start (Store id dst val) = (start + 1, Store start dst val)

labelStmts start (Compound id stmts) = (i, Compound start s)
  where (i,s) = mapAccumL labelStmts (start + 1) stmts

