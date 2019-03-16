module EvalAst where

import Ast
import Hapstone.Internal.X86 as X86
import Util
import Data.Bits

data ExecutionContext = ExecutionContext {
  registers :: [(X86.X86Reg, Int)],
  memory :: [(Int, Int)]
}

newX86Context :: ExecutionContext
newX86Context = ExecutionContext {
  registers = [],
  memory = []
}

-- Evaluates the given node in the given context and returns the result

eval :: ExecutionContext -> AstNode -> Int

eval cin (BvNode a _) = convert a

eval cin (BvxorNode a b) = convert (xor (eval cin a) (eval cin b))

eval cin (BvandNode a b) = convert ((eval cin a) .&. (eval cin b))

eval cin (BvorNode a b) = convert ((eval cin a) .|. (eval cin b))

eval cin (BvaddNode a b) = convert ((eval cin a) + (eval cin b))

eval cin (BvsubNode a b) = convert ((eval cin a) - (eval cin b))

eval cin (ExtractNode a b c) =
  convert ((shift (eval cin c) (convert (-b))) .&. ((2 ^ convert (a + 1 - b)) - 1))

eval cin (GetReg reg) =
  case lookup reg (registers cin) of
    Just x -> x
    Nothing -> error "Invalid x86 context."

-- Executes the given statement in the given context and returns a new context

--exec :: ExecutionContext -> AstNode -> ExecutionContext

-- by default all undefined regs are symbolic?
symbolicEval :: ExecutionContext -> [AstNode] -> ExecutionContext
symbolicEval cin ast =
          cin
