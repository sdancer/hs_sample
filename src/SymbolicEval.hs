module SymbolicEval where

import Ast
import Hapstone.Internal.X86 as X86
import Util
import Data.Bits
import Data.List
import Data.Maybe
import Hapstone.Internal.Capstone as Capstone
import BitVector
import Control.Monad.State.Lazy

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

-- Looks up the byte-sized expression at the given address of the byte-addressed memory.

lookupExpr :: MonadIO m => [(Expr, Expr)] -> Expr -> m Expr

lookupExpr [] addr = return $ Load byte_size_bit addr

lookupExpr ((a, b) : e) addr = do
  result <- exprEquals a addr
  case result of
    Nothing -> fail "Could not determine the equality of two symbolic addresses."
    Just False -> lookupExpr e addr
    Just True -> return b

-- Gets the specified bytes from memory

getMemoryValue :: MonadIO m => [(Expr, Expr)] -> Expr -> Int -> m Expr

getMemoryValue mem a exprSize =
  let byteCount = div exprSize byte_size_bit
      setByte expr offset = do
        let currentAddress = BvaddExpr a (BvExpr $ intToBv offset (getExprSize a))
        contents <- lookupExpr mem currentAddress
        return $ ReplaceExpr (offset*byte_size_bit) expr contents
  in {-simplifyExpr $-} foldM setByte (UndefinedExpr exprSize) [0..byteCount-1]

-- Puts the given byte-sized expression into byte-addressed memory at the given address.

assignExpr :: MonadIO m => [(Expr, Expr)] -> (Expr, Expr) -> m [(Expr, Expr)]

assignExpr [] (a, b) = return $ [(a, b)]

assignExpr ((c, d) : e) (a, b) = do
  result <- exprEquals c a
  case result of
    Nothing -> fail "Could not determine the equality of two symbolic addresses."
    Just False -> do
      rst <- assignExpr e (a, b)
      return ((c, d) : rst)
    Just True -> return $ ((a, b) : e)

-- Put the given expression into memory starting at the given address and return the new
-- context.

updateMemory :: MonadIO m => [(Expr, Expr)] -> (Expr, Expr) -> m [(Expr, Expr)]

updateMemory memory (address, value) =
  let bc = div (getExprSize value) byte_size_bit
      updateByte mem x =
        assignExpr mem (BvaddExpr address (BvExpr $ intToBv x (getExprSize address)),
          simplifyExpr $ ExtractExpr (x*byte_size_bit) ((x+1)*byte_size_bit) value)
  in foldM updateByte memory [0..bc-1]

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

simplifyExprAux (EqualExpr (BvExpr abv) (BvExpr bbv)) = BvExpr (if bvequal abv bbv then bvone abv else bvzero abv)

simplifyExprAux (BvaddExpr (BvExpr abv) (BvExpr bbv)) = BvExpr (bvadd abv bbv)

simplifyExprAux (BvaddExpr a (BvExpr b)) | bvequal b (bvzero b) = a

simplifyExprAux (BvaddExpr (BvExpr b) a) | bvequal b (bvzero b) = a

simplifyExprAux (BvsubExpr (BvExpr abv) (BvExpr bbv)) = BvExpr (bvsub abv bbv)

simplifyExprAux (BvlshrExpr (BvExpr abv) (BvExpr bbv)) = BvExpr (bvlshr abv bbv)

simplifyExprAux (ZxExpr a (BvExpr bbv)) = BvExpr (zx a bbv)

simplifyExprAux (ZxExpr a e) | a == getExprSize e = e

simplifyExprAux (IteExpr (BvExpr a) b c) = if bvequal a (bvzero a) then c else b
-- The entire expression is being replaced
simplifyExprAux (ReplaceExpr l a b) | getExprSize a == getExprSize b = b
-- Join together two adjacent replacements
simplifyExprAux (ReplaceExpr l (ReplaceExpr m b (ExtractExpr n p q)) (ExtractExpr r t u))
    | l == m + (p-n) && p == r && q == u =
  ReplaceExpr m b (ExtractExpr n t q)
simplifyExprAux (ReplaceExpr l (ReplaceExpr m b (BvExpr n)) (BvExpr r)) | l == m + (bvlength n) =
  ReplaceExpr m b (BvExpr (bvconcat r n))
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

simplifyExprAux (Load a b) = Load a (simplifyExpr b)

simplifyExprAux expr = expr

-- Simplifies the entire given expression

simplifyExpr :: Expr -> Expr

simplifyExpr expr =
  let newExpr = mapExpr simplifyExprAux expr
  in if newExpr == expr then expr else simplifyExpr newExpr

-- If the supplied expression is GetReg or Load, then substitute it for its value.
-- Otherwise leave the expression unchanged.

substituteAbsAux :: MonadIO m => SymExecutionContext -> Expr -> m Expr

substituteAbsAux cin (GetReg bs) = return $ getRegisterValue (absoluteRegisterFile cin) bs

substituteAbsAux cin (Load a memStart) = do
  memStartSubs <- substituteAbs cin memStart
  getMemoryValue (absoluteMemory cin) (simplifyExpr memStartSubs) a

substituteAbsAux cin expr = return expr

substituteAbs :: MonadIO m => SymExecutionContext -> Expr -> m Expr

substituteAbs cin expr = mapMExpr (substituteAbsAux cin) expr

substituteRelAux :: MonadIO m => SymExecutionContext -> Expr -> m Expr

substituteRelAux cin (GetReg bs) =
  return $ case getRegisterValue (relativeRegisterFile cin) bs of
    BvExpr v -> BvExpr v
    ReferenceExpr a b -> ReferenceExpr a b
    _ -> GetReg bs

substituteRelAux cin (Load a memStart) = do
  memStartSubs <- substituteAbs cin memStart
  mv <- getMemoryValue (relativeMemory cin) (simplifyExpr memStartSubs) a
  return $ case simplifyExpr mv of
    BvExpr v -> BvExpr v
    ReferenceExpr a b -> ReferenceExpr a b
    _ -> Load a memStart

substituteRelAux cin expr = return expr

substituteRel :: MonadIO m => SymExecutionContext -> Expr -> m Expr

substituteRel cin expr = mapMExpr (substituteRelAux cin) expr

-- Monadic variant of mapAccumL.

mapAccumM :: Monad m => (a -> b -> m (a, c)) -> a -> [b] -> m (a, [c])

mapAccumM f a [] = return (a, [])

mapAccumM f a (l:ls) = do
  (b,c) <- f a l
  (d,e) <- mapAccumM f b ls
  return (d,c:e)

-- Symbolically executes the statement on the given context, potentially simplifying it in
-- the process. Put the result of the simplification or a self-reference into storage.
-- Returns resulting context.

symExec :: MonadIO m => SymExecutionContext -> Stmt Int -> m (SymExecutionContext, Stmt Int)

symExec cin (SetReg id bs a) = do
  relExpr <- simplifyExpr <$> substituteRel cin a
  absExpr <- simplifyExpr <$> substituteAbs cin a
  return (cin { absoluteRegisterFile = setRegisterValue (absoluteRegisterFile cin) bs absExpr,
            relativeRegisterFile = setRegisterValue (relativeRegisterFile cin) bs relExpr },
        SetReg id bs relExpr)

-- add ro memory (raise exception if written)

symExec cin (Store id dst val) = do
  pdestRel <- simplifyExpr <$> substituteRel cin dst
  pdestAbs <- simplifyExpr <$> substituteAbs cin dst
  pvalAbs <- simplifyExpr <$> substituteAbs cin val
  pvalRel <- simplifyExpr <$> substituteRel cin val
  absoluteMemoryV <- updateMemory (absoluteMemory cin) (pdestAbs, pvalAbs)
  relativeMemoryV <- updateMemory (relativeMemory cin) (pdestAbs, pvalRel)
  return (cin { absoluteMemory = absoluteMemoryV, relativeMemory = relativeMemoryV },
        Store id pdestRel pvalRel)

symExec cin (Comment str) = return (cin, Comment str)

symExec cin (Compound id stmts) = do
  (i,s) <- mapAccumM symExec cin stmts
  return (i, Compound id s)

-- Labels all the statements in the instructions with a new unique identifier

labelStmts :: Int -> Stmt a -> (Int, Stmt Int)

labelStmts start (SetReg id bs a) = (start + 1, SetReg start bs a)

labelStmts start (Store id dst val) = (start + 1, Store start dst val)
-- Comments cannot be referenced, hence they do not need labels
labelStmts start (Comment str) = (start, Comment str)

labelStmts start (Compound id stmts) = (i, Compound start s)
  where (i,s) = mapAccumL labelStmts (start + 1) stmts

