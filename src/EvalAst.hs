module EvalAst where

import Ast
import Hapstone.Internal.X86 as X86
import Util
import Data.Bits
import Debug.Trace

-- Represents the state of a processor: register file contents, memory contents, the
-- instructions being executed, and the instruction pointer contents.

data ExecutionContext = ExecutionContext {
  reg_file :: [Int],
  memory :: [(Int, Int)],
  stmts :: [(Label,Stmt)],
  insn_ptr :: Ast.Label
} deriving (Eq, Show)

-- Creates a context where the instruction pointer points to an instruction, and memory
-- and the register file are uninitialized.

uninitializedX86Context :: [(Label,Stmt)] -> Label -> ExecutionContext

uninitializedX86Context stmts insn_ptr = ExecutionContext {
  memory = [],
  reg_file = replicate reg_file_bytes 0,
  stmts = stmts,
  insn_ptr = insn_ptr
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

eval cin (Read a b) =
  let memStart = eval cin b
    in getMemoryValue (memory cin) [memStart..(memStart + a - 1)]

-- Replace the given index of the given list with the given value

replace :: [a] -> Int -> a -> [a]

replace (_:xs) 0 val = val:xs

replace (x:xs) idx val = x:(replace xs (idx - 1) val)

-- Assigns the given value to the given key. Adds a new association to the list if necessary

assign :: Eq a => [(a,b)] -> (a, b) -> [(a, b)]

assign [] (a, b) = [(a, b)]

assign ((c, d) : es) (a, b) | c == a = (a, b) : es

assign ((c, d) : es) (a, b) | c /= a = (c, d) : assign es (a, b)

-- Gets the successor of the given element in the given list.

after :: Eq a => [a] -> a -> a

after (x:y:ys) z | x == z = y

after (x:xs) y = after xs y

exec_aux :: ExecutionContext -> Stmt -> ExecutionContext

-- Executes a SetReg operation by setting each byte of the register separately

exec_aux cin (SetReg bs a) =
  let (insn_ptrs, _) = unzip (stmts cin)
      update_reg_file regs [] _ = regs
      update_reg_file regs (c:cs) val =
        update_reg_file (replace regs c (val .&. ((2 ^ byte_size_bit) - 1))) cs (shift val (-byte_size_bit))
  in ExecutionContext {
    reg_file = update_reg_file (reg_file cin) bs (eval cin a),
    memory = memory cin,
    stmts = stmts cin,
    insn_ptr = after insn_ptrs (insn_ptr cin)
  }

-- Executes a Store operation by setting each byte of memory separately

exec_aux cin (Store n dst val) =
  let (insn_ptrs, _) = unzip (stmts cin)
      updateMemory mem 0 _ _ = mem
      updateMemory mem c d v =
        updateMemory (assign mem (d, (v .&. ((2 ^ byte_size_bit) - 1)))) (c - 1) (d + 1) (shift v (-byte_size_bit))
  in ExecutionContext {
    reg_file = reg_file cin,
    memory = updateMemory (memory cin) n (eval cin dst) (eval cin val),
    stmts = stmts cin,
    insn_ptr = after insn_ptrs (insn_ptr cin)
  }

-- Executes a Branch operation by changing the instruction pointer

exec_aux cin (Branch cond lbl) = ExecutionContext {
    reg_file = reg_file cin,
    memory = memory cin,
    stmts = stmts cin,
    insn_ptr = if eval cin cond /= 0 then (eval cin lbl, 0) else insn_ptr cin
  }

-- Executes the statement pointed to by the instruction pointer and returns the new context

exec :: ExecutionContext -> ExecutionContext

exec cin = case lookup (insn_ptr cin) (stmts cin) of
  Nothing -> error "Instruction pointer has invalid value."
  Just x -> exec_aux cin x

-- Applies the given function on the given argument a given number of times

iter :: (a -> a) -> Int -> a -> a

iter fun 0 x = x

iter fun n x = iter fun (n - 1) (fun x)

-- by default all undefined regs are symbolic?
symbolicEval :: ExecutionContext -> [Expr] -> ExecutionContext
symbolicEval cin ast =
          cin
