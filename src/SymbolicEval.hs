module SymbolicEval where

import Ast
import Hapstone.Internal.X86 as X86
import Util
import Data.Bits
import Data.List
import Data.Maybe
import Hapstone.Internal.Capstone as Capstone
import BitVector
import Data.SBV.Dynamic

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
  -- Holds the contents of the processor registers expressed in terms of initial register and memory values
  absoluteRegisterFile :: SymRegisterFile,
  -- Holds the contents of the processor registers expressed in terms of current register and memory values
  relativeRegisterFile :: SymRegisterFile,
  -- Holds the contents of processor memory expressed in terms of initial register and memory values
  absoluteMemory :: [(Expr, Expr)],
  -- Holds the contents of processor memory expressed in terms of current register and memory values
  relativeMemory :: [(Expr, Expr)],
  -- Holds the processor information that effects interpretation of instructions
  procModes :: [CsMode]
} deriving (Eq, Show)

-- Creates a context where the memory and the register file are empty.

basicX86Context :: [CsMode] -> SymExecutionContext

basicX86Context modes = SymExecutionContext {
  absoluteMemory = [],
  relativeMemory = [],
  absoluteRegisterFile = [],
  relativeRegisterFile = [],
  procModes = modes
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

simplifyExpr :: Expr -> Expr

simplifyExpr expr =
  let newExpr = mapExpr simplifyExprAux expr
  in if newExpr == expr then expr else simplifyExpr newExpr

-- If the supplied expression is GetReg or Load, then substitute it for its value.
-- Otherwise leave the expression unchanged.

substituteAbsAux :: SymExecutionContext -> Expr -> Expr

substituteAbsAux cin (GetReg bs) = getRegisterValue (absoluteRegisterFile cin) bs

substituteAbsAux cin (Load a memStart) = getMemoryValue (absoluteMemory cin) memStart a

substituteAbsAux cin expr = expr

substituteAbs :: SymExecutionContext -> Expr -> Expr

substituteAbs cin expr = mapExpr (substituteAbsAux cin) expr

substituteRelAux :: SymExecutionContext -> Expr -> Expr

substituteRelAux cin (GetReg bs) =
  case getRegisterValue (relativeRegisterFile cin) bs of
    BvExpr v -> BvExpr v
    ReferenceExpr a b -> ReferenceExpr a b
    _ -> GetReg bs

substituteRelAux cin (Load a memStart) =
  case getMemoryValue (relativeMemory cin) memStart a of
    BvExpr v -> BvExpr v
    ReferenceExpr a b -> ReferenceExpr a b
    _ -> Load a memStart

substituteRelAux cin expr = expr

substituteRel :: SymExecutionContext -> Expr -> Expr

substituteRel cin expr = mapExpr (substituteRelAux cin) expr

exprToSVal :: Expr -> SVal

exprToSVal (BvExpr a) = svInteger (KBounded False (bvlength a)) (toInteger (bvToInt a))

exprToSVal (BvaddExpr a b) = svPlus (exprToSVal a) (exprToSVal b)

exprToSVal (BvandExpr a b) = svAnd (exprToSVal a) (exprToSVal b)

exprToSVal (BvashrExpr a b) = svShiftRight (svSign (exprToSVal a)) (exprToSVal b)

exprToSVal (BvlshrExpr a b) = svShiftRight (svUnsign (exprToSVal a)) (exprToSVal b)

exprToSVal (BvmulExpr a b) = svTimes (exprToSVal a) (exprToSVal b)

exprToSVal (BvnegExpr a) = svUNeg (exprToSVal a)

exprToSVal (BvnotExpr a) = svNot (exprToSVal a)

exprToSVal (BvorExpr a b) = svOr (exprToSVal a) (exprToSVal b)

exprToSVal (BvrolExpr a b) = svRotateLeft (exprToSVal a) (exprToSVal b)

exprToSVal (BvrorExpr a b) = svRotateRight (exprToSVal a) (exprToSVal b)

exprToSVal (BvsdivExpr a b) = svQuot (svSign $ exprToSVal a) (svSign $ exprToSVal b)

exprToSVal (BvsgeExpr a b) = svGreaterEq (svSign $ exprToSVal a) (svSign $ exprToSVal b)

exprToSVal (BvsgtExpr a b) = svGreaterThan (svSign $ exprToSVal a) (svSign $ exprToSVal b)

exprToSVal (BvshlExpr a b) = svShiftLeft (exprToSVal a) (exprToSVal b)

exprToSVal (BvsleExpr a b) = svLessEq (svSign $ exprToSVal a) (svSign $ exprToSVal b)

exprToSVal (BvsltExpr a b) = svLessThan (svSign $ exprToSVal a) (svSign $ exprToSVal b)

exprToSVal (BvsremExpr a b) = svRem (svSign $ exprToSVal a) (svSign $ exprToSVal b)

exprToSVal (BvsubExpr a b) = svMinus (exprToSVal a) (exprToSVal b)

exprToSVal (BvudivExpr a b) = svQuot (svUnsign $ exprToSVal a) (svUnsign $ exprToSVal b)

exprToSVal (BvugeExpr a b) = svGreaterEq (svUnsign $ exprToSVal a) (svUnsign $ exprToSVal b)

exprToSVal (BvugtExpr a b) = svGreaterThan (svUnsign $ exprToSVal a) (svUnsign $ exprToSVal b)

exprToSVal (BvuleExpr a b) = svLessEq (svUnsign $ exprToSVal a) (svUnsign $ exprToSVal b)

exprToSVal (BvultExpr a b) = svLessThan (svUnsign $ exprToSVal a) (svUnsign $ exprToSVal b)

exprToSVal (BvuremExpr a b) = svRem (svUnsign $ exprToSVal a) (svUnsign $ exprToSVal b)

exprToSVal (BvxorExpr a b) = svXOr (exprToSVal a) (exprToSVal b)

exprToSVal (EqualExpr a b) = svEqual (exprToSVal a) (exprToSVal b)

exprToSVal (IteExpr a b c) = svIte (exprToSVal a) (exprToSVal b) (exprToSVal c)

exprToSVal (ExtractExpr a b c) = svExtract (b-1) a (exprToSVal c)

{-  
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
  deriving (Eq, Show)-}

-- Put the given expression into memory starting at the given address and return the new
-- context.

updateMemory :: [(Expr, Expr)] -> Expr -> Expr -> [(Expr, Expr)]

updateMemory memory address value =
  let bc = div (getExprSize value) byte_size_bit
      updateByte mem x =
        assign mem (BvaddExpr address (BvExpr $ intToBv x (getExprSize address)),
          simplifyExpr $ ExtractExpr (x*byte_size_bit) ((x+1)*byte_size_bit) value)
  in foldl updateByte memory [0..bc-1]

-- Symbolically executes the statement on the given context, potentially simplifying it in
-- the process. Put the result of the simplification or a self-reference into storage.
-- Returns resulting context.

symExec :: SymExecutionContext -> Stmt Int -> (SymExecutionContext, Stmt Int)

symExec cin (SetReg id bs a) =
  let relExpr = simplifyExpr $ substituteRel cin a
      absExpr = simplifyExpr $ substituteAbs cin a
  in (cin { absoluteRegisterFile = setRegisterValue (absoluteRegisterFile cin) bs absExpr,
            relativeRegisterFile = setRegisterValue (relativeRegisterFile cin) bs relExpr },
        SetReg id bs relExpr)

-- add ro memory (raise exception if written)

symExec cin (Store id dst val) =
  let pdest = simplifyExpr $ substituteAbs cin dst
      pvalAbs = simplifyExpr $ substituteAbs cin val
      pvalRel = simplifyExpr $ substituteRel cin val
  in (cin { absoluteMemory = updateMemory (absoluteMemory cin) pdest pvalAbs,
            relativeMemory = updateMemory (relativeMemory cin) pdest pvalRel },
        Store id pdest pvalRel)

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

