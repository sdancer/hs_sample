module AstTools where

import Ast



filter_out_flags :: [[AstNodeType]] -> [[AstNodeType]]
filter_out_flags asttree =
  --just used for some test, shouldn't be used in code as its not correct
  filter asttree (top most != SetFlag)
