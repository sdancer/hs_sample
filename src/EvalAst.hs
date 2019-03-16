module EvalAst where

import Ast
import Hapstone.Internal.X86 as X86

-- data Context = {
--       registers = [...],
--       memory = [...]
-- }

import qualified Data.Map.Strict as Map

data MemRegions = Lol

data ExecutionContext = ExecutionContext {
        registers :: (Map.Map X86.X86Reg Int)
      , memory :: [MemRegions]
}

newX86Context :: ExecutionContext
newX86Context =
                ExecutionContext {
                  registers = Map.empty,
                  memory = []
                }

eval :: ExecutionContext -> [AstNode] -> ExecutionContext
eval cin ast =
          cin

-- by default all undefined regs are symbolic?
symbolicEval :: ExecutionContext -> [AstNode] -> ExecutionContext
symbolicEval cin ast =
          cin
