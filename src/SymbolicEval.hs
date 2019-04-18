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

-- Gets the value of the specified compound register from the register file

getRegisterValue :: SymRegisterFile -> CompoundReg -> Expr

getRegisterValue regFile reg =
  let rootRegister = getRootRegister reg
      (l,h) = registerSub reg rootRegister
  in case lookup rootRegister regFile of
    Just x -> simplifyExpr $ ExtractExpr l h x
    Nothing -> GetReg reg

-- Updates the given register file by putting the given value in the given register

setRegisterValue :: SymRegisterFile -> CompoundReg -> Expr -> SymRegisterFile

setRegisterValue regFile reg val =
  let rootRegister = getRootRegister reg
      (l,h) = registerSub reg rootRegister
      newRootExpr = simplifyExpr $ ReplaceExpr l (getRegisterValue regFile rootRegister) val
  in assign regFile (rootRegister, newRootExpr)

-- Updates the register file by removing the value in the given register

unsetRegisterValue :: SymRegisterFile -> CompoundReg -> SymRegisterFile

unsetRegisterValue regFile reg = setRegisterValue regFile reg (UndefinedExpr $ getRegisterSize reg)

-- Get the register values from the register file

getRegisterValues :: SymRegisterFile -> [(X86.X86Reg, Expr)]

getRegisterValues regFile =
  map (\(x, y) -> (x, getRegisterValue regFile y)) (filter (isRegisterDefined regFile . snd) x86RegisterMap)

-- Gets the specified bytes from memory

getMemoryValue :: [(Expr, Expr)] -> Expr -> Int -> Expr

getMemoryValue mem a exprSize =
  let byteCount = div exprSize byte_size_bit
      setByte expr offset =
        let currentAddress = BvaddExpr a (BvExpr $ intToBv offset (getExprSize a))
        in ReplaceExpr (offset*byte_size_bit) expr $
          case lookup currentAddress mem of
            Just x -> x
            _ -> Load byte_size_bit currentAddress
  in simplifyExpr $ foldl setByte (UndefinedExpr exprSize) [0..byteCount-1]

-- Represents the state of a processor: register file contents, data memory contents, and
-- the instruction memory.

data SymExecutionContext = SymExecutionContext {
  reg_file :: SymRegisterFile, -- Holds the contents and validity of the processor registers
  memory :: [(Expr, Expr)], -- Holds the contents and validity of the processor memory
  proc_modes :: [CsMode] -- Holds the processor information that effects interpretation of instructions
} deriving (Eq, Show)

-- Creates a context where the memory and the register file are empty.

basicX86Context :: [CsMode] -> SymExecutionContext

basicX86Context modes = SymExecutionContext {
  memory = [],
  reg_file = emptyRegisterFile,
  proc_modes = modes
}

-- Simplifies root of the given expression

simplifyExprAux :: Expr -> Expr

simplifyExprAux (BvxorExpr (BvExpr abv) (BvExpr bbv)) = BvExpr (bvxor abv bbv)

simplifyExprAux (BvandExpr (BvExpr abv) (BvExpr bbv)) = BvExpr (bvand abv bbv)

simplifyExprAux (BvorExpr (BvExpr abv) (BvExpr bbv)) = BvExpr (bvor abv bbv)

simplifyExprAux (BvnotExpr (BvExpr abv)) = BvExpr (bvnot abv)

simplifyExprAux (EqualExpr (BvExpr abv) (BvExpr bbv)) = BvExpr (if equal abv bbv then one abv else zero abv)

simplifyExprAux (BvaddExpr (BvExpr abv) (BvExpr bbv)) = BvExpr (bvadd abv bbv)

simplifyExprAux (BvsubExpr (BvExpr abv) (BvExpr bbv)) = BvExpr (bvsub abv bbv)

simplifyExprAux (BvlshrExpr (BvExpr abv) (BvExpr bbv)) = BvExpr (bvlshr abv bbv)

simplifyExprAux (ZxExpr a (BvExpr bbv)) = BvExpr (zx a bbv)

simplifyExprAux (ZxExpr a e) | a == getExprSize e = e

simplifyExprAux (IteExpr (BvExpr a) b c) = if equal a (zero a) then c else b
-- The entire expression is being replaced
simplifyExprAux (ReplaceExpr l a b) | getExprSize a == getExprSize b = b
-- Join together two adjacent replacements
simplifyExprAux (ReplaceExpr l (ReplaceExpr m b (ExtractExpr n p q)) (ExtractExpr r t u))
    | l == m + (p-n) && p == r && q == u =
  ReplaceExpr m b (ExtractExpr n t q)
-- A part of a literal is being replaced by another literal
simplifyExprAux (ReplaceExpr l (BvExpr a) (BvExpr b)) = BvExpr $ bvreplace a l b
-- The current replacement coincides with a previous replacement
simplifyExprAux (ReplaceExpr l (ReplaceExpr a b c) e) | l == a && getExprSize e == getExprSize c =
  ReplaceExpr l b e
-- The extraction is the entire expression
simplifyExprAux (ExtractExpr l h e) | h - l == getExprSize e = e
-- The extraction is being done on a literal
simplifyExprAux (ExtractExpr l h (BvExpr a)) = BvExpr $ bvextract l h a
-- The extraction is within the replacement expression
simplifyExprAux (ExtractExpr l h (ReplaceExpr a e f)) | a <= l && h <= a + getExprSize f =
  ExtractExpr y z f where y = l - a; z = h - a
-- The replacement expression is disjoint from the extraction
simplifyExprAux (ExtractExpr l h (ReplaceExpr a e f)) | a + getExprSize f <= l || h <= a =
  ExtractExpr l h e

simplifyExprAux expr = expr

-- Simplifies the entire given expression

simplifyExpr = mapExpr simplifyExprAux

-- If the supplied expression is GetReg or Load, then substitute it for its value.
-- Otherwise leave the expression unchanged.

substituteStorage :: SymExecutionContext -> Expr -> Expr

substituteStorage cin (GetReg bs) = getRegisterValue (reg_file cin) bs

-- add ro memory (raise exception if written)

substituteStorage cin (Load a memStart) = getMemoryValue (memory cin) memStart a

substituteStorage cin expr = expr

-- Substitute in all known values and simplify. The simplifying may cause a memory address
-- to change from some expression to a literal. Hence the simplification should be
-- followed by another attempt to substitute and simplify, ...

substituteSimplify :: SymExecutionContext -> Expr -> Expr

substituteSimplify cin expr =
  let nextExpr = simplifyExpr $ mapExpr (substituteStorage cin) expr
  in if nextExpr == expr then nextExpr else substituteSimplify cin nextExpr

-- Put the given expression into memory starting at the given address and return the new
-- context.

updateMemory :: SymExecutionContext -> Expr -> Expr -> SymExecutionContext

updateMemory cin address val =
  let bc = div (getExprSize val) byte_size_bit
      updateByte mem x =
        assign mem (BvaddExpr address (BvExpr $ intToBv x (getExprSize address)),
          simplifyExpr $ ExtractExpr (x*byte_size_bit) ((x+1)*byte_size_bit) val)
      nmem = foldl updateByte (memory cin) [0..bc-1]
  in cin { memory = nmem }

-- Backdates the given expression over the given statement. That is, makes an expression
-- that evaluates to the same value in the environment that prevails just before the
-- given statement is executed.

{-backdate :: [Stmt a] -> Expr -> Expr

backdate [SetReg _ r1 a : _] expr =
  let substitute (GetReg r2) = simplifyExpr $ ReplaceExpr l1 (GetReg r2) a
        where (l1,_) = registerSub r1 r2
      substitute x = x
  in mapExpr substitute expr

backdate (Store _ dst val) expr =
  let substitute (Load bc pdst) | exprEquals pdst dst == Just True = val
      substitute (Load bc pdst) | exprEquals pdst dst == Just False = val
      substitute x = x
  in mapExpr substitute expr

backdate [Comment _ : _] expr = expr

backdate [Compound _ stmts : _] expr = foldr backdate expr stmts-}

-- Symbolically executes the statement on the given context, potentially simplifying it in
-- the process. Put the result of the simplification or a self-reference into storage.
-- Returns resulting context.

symExec :: SymExecutionContext -> Stmt Int -> (SymExecutionContext, Stmt Int)

symExec cin (SetReg id bs a) =
  let exprVal = substituteSimplify cin a
      regVal = case exprVal of
        BvExpr v -> BvExpr v
        _ -> GetReg bs
  in (cin { reg_file = setRegisterValue (reg_file cin) bs regVal }, SetReg id bs exprVal)

symExec cin (Store id dst val) =
  let pdest = substituteSimplify cin dst
      pval = substituteSimplify cin val
      memVal = case pval of
        BvExpr v -> BvExpr v
        _ -> Load (getExprSize val) pdest
  in (updateMemory cin pdest memVal, Store id pdest pval)

symExec cin (Comment str) = (cin, Comment str)

symExec cin (Compound id stmts) = (i, Compound id s)
  where (i,s) = mapAccumL symExec cin stmts

-- Labels all the statements in the instructions with a new unique identifier

labelStmts :: Int -> Stmt a -> (Int, Stmt Int)

labelStmts start (SetReg id bs a) = (start + 1, SetReg start bs a)

labelStmts start (Store id dst val) = (start + 1, Store start dst val)
-- Comments cannot be referenced, hence they do not need labels
labelStmts start (Comment str) = (start, Comment str)

labelStmts start (Compound id stmts) = (i, Compound start s)
  where (i,s) = mapAccumL labelStmts (start + 1) stmts

