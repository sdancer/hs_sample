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

getRegisterParent :: SymRegisterFile -> CompoundReg -> Maybe CompoundReg

getRegisterParent regFile reg = find (isSubregisterOf reg . fst) regFile >>= Just . fst

-- Gets the value of the specified compound register from the register file

getRegisterValue :: SymRegisterFile -> CompoundReg -> Maybe Expr

getRegisterValue regFile reg =
  case getRegisterParent regFile reg of
    Just parentReg ->
      let (l, h) = registerSub reg parentReg
      in Just $ ExtractExpr l h $ fromJust $ lookup parentReg regFile
    Nothing -> Nothing

-- Updates the given register file by putting the given value in the given register

setRegisterValue :: SymRegisterFile -> CompoundReg -> Expr -> SymRegisterFile

setRegisterValue reg_file reg val =
  let (ranges, _) = unzip reg_file
      new_ranges = addRegister ranges reg
      undef_reg_file = map (\x -> (x, UndefinedExpr (getRegSize x))) new_ranges
      put reg_file (reg, value) = map (\(x, y) ->
        (x, if isSubregisterOf reg x then (let pos = fst (registerSub reg x) in ReplaceExpr pos y value) else y)) reg_file
  in foldl put undef_reg_file (reg_file ++ [(reg, val)])

-- Updates the register file by removing the value in the given register

unsetRegisterValue :: SymRegisterFile -> CompoundReg -> SymRegisterFile

unsetRegisterValue regFile reg =
  let (ranges, _) = unzip regFile
      newRanges = removeRegister ranges reg
  in map (\x -> (x, fromJust $ getRegisterValue regFile x)) newRanges

-- Get the register values from the register file

getRegisterValues regFile =
  map (\(x, y) -> (x, getRegisterValue regFile y)) (filter (isRegisterDefined regFile . snd) x86RegisterMap)

-- Gets the specified bytes from memory

getMemoryValue :: [(Int, Int)] -> [Int] -> Maybe BitVector

getMemoryValue _ [] = Just empty

getMemoryValue mem (b:bs) =
  case (lookup b mem, getMemoryValue mem bs) of
    (Just x, Just y) -> Just (bvconcat y (intToBv x))
    _ -> Nothing

-- Represents the state of a processor: register file contents, data memory contents, and
-- the instruction memory.

data SymExecutionContext = SymExecutionContext {
  reg_file :: SymRegisterFile, -- Holds the contents and validity of the processor registers
  memory :: [(Int, Int)], -- Holds the contents and validity of the processor memory
  stmts :: [(Int, [Stmt])], -- Holds the instructions to be executed and their memory addresses
  proc_modes :: [CsMode] -- Holds the processor information that effects interpretation of instructions
} deriving (Eq, Show)

-- Creates a context where the memory and the register file are empty.

basicX86Context :: [CsMode] -> [(Int, [Stmt])] -> SymExecutionContext

basicX86Context modes stmts = SymExecutionContext {
  memory = [],
  reg_file = emptyRegisterFile,
  stmts = stmts,
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

symEval cin (GetReg bs) = fromJust $ getRegisterValue (reg_file cin) bs

-- add ro memory (raise exception if written)
-- add symbolic addressed memory (example stack, no concrete values mapping references)

symEval cin (Load a b) =
  case (symEval cin b) of
    (BvExpr memStartBv) ->
      let memStart = bvToInt memStartBv
          memVal = getMemoryValue (memory cin) [memStart..(memStart + a - 1)]
      in case memVal of
        Just x -> BvExpr x
        Nothing -> Load a (BvExpr memStartBv)
    (b) -> Load a b

symEval cin expr = expr

symExec :: SymExecutionContext -> Stmt -> (SymExecutionContext, Stmt)

-- Symbolically executes a SetReg operation by either simplifying assignment value to a
-- literal then putting it into the register file, or by undefining the target register
-- to keep later statements symbolic.

symExec cin (SetReg bs a) =
  let c = symEval cin a
      nreg = setRegisterValue (reg_file cin) bs c
  in (cin { reg_file = nreg }, SetReg bs c)


symExec cin (Store n dst val) =
  let pdest = symEval cin dst
      pval = symEval cin val
      bytes bs = shift bs (0-3)
  in
      case pdest of
        BvExpr a -> (updateMemory cin (bytes (bvlength a)) (bvToInt a) 0x1337, Store n pdest pval)
        _ -> error $ "Store on symbolic mem not implemented"

updateMemory :: SymExecutionContext -> Int -> Int -> Int -> SymExecutionContext

updateMemory cin bc address val =
    let nmem = foldl (\mem x -> assign mem ((address+x), (shift val (0-(8 * x))) .&. 0xff)) (memory cin) [0..bc-1]
    in cin { memory = nmem }

-- Executes a group of statements pointed to by the instruction pointer and returns the
-- new context

symSteps :: SymExecutionContext -> (SymExecutionContext, [(Int, [Stmt])])

symSteps cin | stmts cin == [] = (cin, [])

symSteps cin =
    let x = snd (head (stmts cin)) in
      let process ec [] ns = (ec {
            stmts = tail (stmts ec)
          }, (fst (head (stmts cin)), reverse ns))
          process ec (x:xs) ns =
            let (nec, s) = symExec ec x
            in process nec xs (s:ns)
          (fec, ent) = process cin x []
          (ffec, ents) = symSteps fec
      in (ffec, (ent:ents))

