module AstTools where

import Ast

filter_out_flags :: [[Stmt]] -> [[Stmt]]
filter_out_flags asttree =
  --just used for some test, shouldn't be used in code as its not correct
  map (\x ->
    filter (\y -> case y of
      (SetFlag _ _) -> False
      _ -> True
      ) x
    ) asttree

