module REAstTools where

import Ast

filter_out_flags :: [[AstNodeType]] -> [[AstNodeType]]
filter_out_flags asttree =
  --just used for some test, shouldn't be used in code as its not correct
  map asttree (\x ->
    filter x (\y -> case y of
      (SetFlag _ _) -> false
      _ -> true
    )
