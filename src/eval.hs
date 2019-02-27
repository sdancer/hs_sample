module EvalAst where

import Ast

-- data Context = {
--       registers = [...],
--       memory = [...]
-- }

data ExecutionContext = Lol

eval :: ExecutionContext -> [AstNodeType] -> ExecutionContext
eval cin ast =
          Lol
