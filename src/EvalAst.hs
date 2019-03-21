module EvalAst where

import Ast
import Hapstone.Internal.X86 as X86
import Util
import Data.Bits
import Hapstone.Internal.Capstone as Capstone

-- Represents the state of a processor: register file contents, data memory contents, and
-- the instruction memory.

data ExecutionContext = ExecutionContext {
  reg_file :: [Int],
  memory :: [(Int, Int)],
  stmts :: [(Int, [Stmt])],
  proc_modes :: [CsMode]
} deriving (Eq, Show)

-- Creates a context where the instruction pointer points to the first instruction, and
-- memory and the register file are uninitialized.

uninitializedX86Context :: [CsMode] -> [(Int, [Stmt])] -> ExecutionContext

uninitializedX86Context modes stmts = ExecutionContext {
  memory = [],
  reg_file = update_reg_file (replicate reg_file_bytes 0) (get_insn_ptr modes) (fst (head stmts)),
  stmts = stmts,
  proc_modes = modes
}

-- Evaluates to a bit vector with all 1s up to bit high

oneBitsUpto high = (shift 1 (high + 1)) - 1

-- Evaluates to a bit vector with all 1s between bit low and bit high

oneBitsBetween high low = oneBitsUpto high - (oneBitsUpto (low - 1))

-- Evaluates the given expression in the given context and returns the result

eval :: ExecutionContext -> Expr -> Int

eval cin (BvExpr a _) = a

eval cin (BvxorExpr a b) = xor (eval cin a) (eval cin b)

eval cin (BvandExpr a b) = (eval cin a) .&. (eval cin b)

eval cin (BvorExpr a b) = (eval cin a) .|. (eval cin b)

eval cin (BvnotExpr a) = complement (eval cin a)

eval cin (EqualExpr a b) = if (eval cin a) == (eval cin b) then 1 else 0

eval cin (BvaddExpr a b) = (eval cin a) + (eval cin b)

eval cin (BvsubExpr a b) = (eval cin a) - (eval cin b)

eval cin (BvlshrExpr a b) = convert (shift ((convert (eval cin a)) :: Word) (-(eval cin b)))

eval cin (ZxExpr a b) = eval cin b

eval cin (IteExpr a b c) =
  if (eval cin a) /= 0 then eval cin b
  else eval cin c

eval cin (ReplaceExpr a b c d) =
  ((eval cin c) .&. (complement (oneBitsBetween a b))) .|. shift (eval cin d) b

eval cin (ExtractExpr a b c) = (shift (eval cin c) (-b)) .&. ((2 ^ (a + 1 - b)) - 1)

eval cin (GetReg bs) = getRegisterValue (reg_file cin) bs

eval cin (Load a b) =
  let memStart = eval cin b
    in getMemoryValue (memory cin) [memStart..(memStart + a - 1)]

-- Assigns the given value to the given key. Adds a new association to the list if necessary

assign :: Eq a => [(a,b)] -> (a, b) -> [(a, b)]

assign [] (a, b) = [(a, b)]

assign ((c, d) : es) (a, b) | c == a = (a, b) : es

assign ((c, d) : es) (a, b) | c /= a = (c, d) : assign es (a, b)

exec :: ExecutionContext -> Stmt -> ExecutionContext

-- Executes a SetReg operation by setting each byte of the register separately

exec cin (SetReg bs a) = ExecutionContext {
    reg_file = update_reg_file (reg_file cin) bs (eval cin a),
    memory = memory cin,
    stmts = stmts cin,
    proc_modes = proc_modes cin
  }

-- Executes a Store operation by setting each byte of memory separately

exec cin (Store n dst val) =
  let updateMemory mem 0 _ _ = mem
      updateMemory mem c d v =
        updateMemory (assign mem (d, (v .&. ((2 ^ byte_size_bit) - 1)))) (c - 1) (d + 1) (shift v (-byte_size_bit))
  in ExecutionContext {
    reg_file = reg_file cin,
    memory = updateMemory (memory cin) n (eval cin dst) (eval cin val),
    stmts = stmts cin,
    proc_modes = proc_modes cin
  }

-- Executes a group of statements pointed to by the instruction pointer and returns the
-- new context

step :: ExecutionContext -> ExecutionContext

step cin =
  let procInsnPtr = get_insn_ptr (proc_modes cin)
      registerValue = getRegisterValue (reg_file cin) procInsnPtr
  in case lookup registerValue (stmts cin) of
    Nothing -> error "Instruction pointer has invalid value."
    Just x -> foldl exec cin x

-- Applies the given function on the given argument a given number of times

iter :: (a -> a) -> Int -> a -> a

iter fun 0 x = x

iter fun n x = iter fun (n - 1) (fun x)

-- by default all undefined regs are symbolic?
symbolicEval :: ExecutionContext -> [Expr] -> ExecutionContext
symbolicEval cin ast =
          cin
