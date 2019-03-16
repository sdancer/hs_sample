module EvalAst where

import Ast

-- data Context = {
--       registers = [...],
--       memory = [...]
-- }

import qualified Data.Map.Strict as Map

data MemRegions = Lol

data ExecutionContext = ExecutionContext {
        registers :: (Map.Map Register Int)
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
