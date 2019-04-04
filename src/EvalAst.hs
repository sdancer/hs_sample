module EvalAst where

import Ast
import Hapstone.Internal.X86 as X86
import Util
import Data.Bits
import Hapstone.Internal.Capstone as Capstone
import BitVector

-- Represents the state of a processor: register file contents, data memory contents, and
-- the instruction memory.

data ExecutionContext = ExecutionContext {
  reg_file :: RegisterFile, -- Holds the contents and validity of the processor registers
  memory :: [(Int, Int)], -- Holds the contents and validity of the processor memory
  stmts :: [(Int, [Stmt])], -- Holds the instructions to be executed and their memory addresses
  proc_modes :: [CsMode] -- Holds the processor information that effects interpretation of instructions
} deriving (Eq, Show)

-- Creates a context where the instruction pointer points to the first instruction, and
-- memory and the register file are empty.

basicX86Context :: [CsMode] -> [(Int, [Stmt])] -> ExecutionContext

basicX86Context modes stmts = ExecutionContext {
  memory = [],
  -- Point the instruction pointer to the first instruction on the list
  reg_file = update_reg_file emptyRegisterFile (get_insn_ptr modes) (bitVector (convert (fst (head stmts))) (get_arch_bit_size modes)),
  stmts = stmts,
  proc_modes = modes
}

-- Evaluates the given expression in the given context and returns the result

eval :: ExecutionContext -> Expr -> BitVector

eval cin (BvExpr a) = a

eval cin (BvxorExpr a b) = bvxor (eval cin a) (eval cin b)

eval cin (BvandExpr a b) = bvand (eval cin a) (eval cin b)

eval cin (BvorExpr a b) = bvor (eval cin a) (eval cin b)

eval cin (BvnotExpr a) = bvnot (eval cin a)

eval cin (EqualExpr a b) =
  let abv = eval cin a
      bbv = eval cin b
  in if equal abv bbv then one abv else zero abv

eval cin (BvaddExpr a b) = bvadd (eval cin a) (eval cin b)

eval cin (BvsubExpr a b) = bvsub (eval cin a) (eval cin b)

eval cin (BvlshrExpr a b) = bvlshr (eval cin a) (eval cin b)

eval cin (ZxExpr a b) = zx a (eval cin b)

eval cin (IteExpr a b c) =
  let abv = eval cin a in if equal abv (zero abv) then eval cin c else eval cin b

eval cin (ReplaceExpr b c d) = bvreplace (eval cin c) b (eval cin d)

eval cin (ExtractExpr a b c) = bvextract a b (eval cin c)

eval cin (GetReg bs) =
  case getRegisterValue (reg_file cin) bs of
    Nothing -> error "Read attempted on uninitialized memory."
    Just x -> x

eval cin (Load a b) =
  let memStart = bvToInt (eval cin b)
      memVal = getMemoryValue (memory cin) [memStart..(memStart + a - 1)]
  in case memVal of
    Nothing -> error "Read attempted on uninitialized memory."
    Just x -> x

-- Assigns the given value to the given key. Adds a new association to the list if necessary

assign :: Eq a => [(a,b)] -> (a, b) -> [(a, b)]

assign [] (a, b) = [(a, b)]

assign ((c, d) : es) (a, b) | c == a = (a, b) : es

assign ((c, d) : es) (a, b) | c /= a = (c, d) : assign es (a, b)

exec :: ExecutionContext -> Stmt -> ExecutionContext

-- Executes a SetReg operation by setting each byte of the register separately

exec cin (SetReg bs a) =
  cin { reg_file = update_reg_file (reg_file cin) bs (eval cin a) }

-- Executes a Store operation by setting each byte of memory separately

exec cin (Store n dst val) =
  let updateMemory mem 0 _ _ = mem
      updateMemory mem c d v =
        updateMemory (assign mem (d, (v .&. (bit byte_size_bit - 1)))) (c - 1) (d + 1) (shift v (-byte_size_bit))
  in cin { memory = updateMemory (memory cin) n (bvToInt (eval cin dst)) (bvToInt (eval cin val)) }

-- Executes a group of statements pointed to by the instruction pointer and returns the
-- new context

step :: ExecutionContext -> ExecutionContext

step cin =
  let procInsnPtr = get_insn_ptr (proc_modes cin)
  in case getRegisterValue (reg_file cin) procInsnPtr of
    Nothing -> error "Instruction pointer has not yet been set."
    Just registerValue ->
      case lookup (bvToInt registerValue) (stmts cin) of
        Nothing -> error "Instruction pointer has invalid value."
        Just x -> foldl exec cin x

-- Applies the given function on the given argument a given number of times

iter :: (a -> a) -> Int -> a -> a

iter fun 0 x = x

iter fun n x = iter fun (n - 1) (fun x)

