module EvalAst where

import Ast
import Hapstone.Internal.X86 as X86
import Util
import Data.Bits

data ExecutionContext = ExecutionContext {
  reg_file :: [Int],
  memory :: [(Int, Int)]
} deriving (Eq, Show, Read)

x86Context :: ExecutionContext
x86Context = ExecutionContext {
  reg_file = replicate reg_file_bytes 0,
  memory = []
}

-- Evaluates to a bit vector with all 1s up to bit high

oneBitsUpto high = (shift 1 (high + 1)) - 1

-- Evaluates to a bit vector with all 1s between bit low and bit high

oneBitsBetween high low = oneBitsUpto high - (oneBitsUpto (low - 1))

-- Evaluates the given node in the given context and returns the result

eval :: ExecutionContext -> AstNode -> Int

eval cin (BvNode a _) = convert a

eval cin (BvxorNode a b) = convert (xor (eval cin a) (eval cin b))

eval cin (BvandNode a b) = convert ((eval cin a) .&. (eval cin b))

eval cin (BvorNode a b) = convert ((eval cin a) .|. (eval cin b))

eval cin (BvnotNode a) = convert (complement (eval cin a))

eval cin (EqualNode a b) = if (eval cin a) == (eval cin b) then 1 else 0

eval cin (BvaddNode a b) = convert ((eval cin a) + (eval cin b))

eval cin (BvsubNode a b) = convert ((eval cin a) - (eval cin b))

eval cin (BvlshrNode a b) = convert (shift ((convert (eval cin a)) :: Word) (-(eval cin b)))

eval cin (ZxNode a b) = convert (eval cin b)

eval cin (IteNode a b c) =
  if (eval cin a) /= 0 then eval cin b
  else eval cin c

eval cin (ReplaceNode a b c d) =
  ((eval cin c) .&. (complement (oneBitsBetween (convert a) (convert b))))
    .|. convert (shift (eval cin d) (convert b))

eval cin (ExtractNode a b c) =
  convert ((shift (eval cin c) (convert (-b))) .&. ((2 ^ convert (a + 1 - b)) - 1))

eval cin (GetReg bs) = getRegisterValue (reg_file cin) bs

eval cin (Read a b) =
  let memStart = eval cin b
    in getMemoryValue (memory cin) [memStart..(memStart + (convert a) -1)]

-- Replace the given index of the given list with the given value

replace :: [a] -> Int -> a -> [a]

replace (_:xs) 0 val = val:xs

replace (x:xs) idx val = x:(replace xs (idx - 1) val)

-- Assigns the given value to the given key. Adds a new association to the list if necessary

assign :: Eq a => [(a,b)] -> (a, b) -> [(a, b)]

assign [] (a, b) = [(a, b)]

assign ((c, d) : es) (a, b) | c == a = (a, b) : es

assign ((c, d) : es) (a, b) | c /= a = (c, d) : assign es (a, b)

-- Executes the given statement in the given context and returns a new context

exec :: ExecutionContext -> Stmt -> ExecutionContext

-- Executes a SetReg operation by setting each byte of the register separately

exec cin (SetReg bs a) =
  let update_reg_file regs [] _ = regs
      update_reg_file regs (c:cs) val =
        update_reg_file (replace regs c (val .&. ((2 ^ byte_size_bit) - 1))) cs (shift val (-byte_size_bit))
  in ExecutionContext {
    reg_file = update_reg_file (reg_file cin) bs (eval cin a),
    memory = memory cin
  }

-- Executes a Store operation by setting each byte of memory separately

exec cin (Store n dst val) =
  let updateMemory mem 0 _ _ = mem
      updateMemory mem c d v =
        updateMemory (assign mem (d, (v .&. ((2 ^ byte_size_bit) - 1)))) (c - 1) (d + 1) (shift v (-byte_size_bit))
  in ExecutionContext {
    reg_file = reg_file cin,
    memory = updateMemory (memory cin) n (eval cin dst) (eval cin val)
  }

-- Executes given list of statements in order in the given context and returns a new context

run :: ExecutionContext -> [Stmt] -> ExecutionContext

run cin ss = foldl exec cin ss

-- by default all undefined regs are symbolic?
symbolicEval :: ExecutionContext -> [AstNode] -> ExecutionContext
symbolicEval cin ast =
          cin
