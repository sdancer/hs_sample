module Phasses where

import Data.List

data SG = Set Int Int | Get Int deriving (Show, Eq)

naive_dead_code_elimination =
  let folder ele (acc1, acc2) =
        let cond_add a = case (elem a acc2) of
                      True -> (acc1, acc2) -- skipped
                      False -> ([ele] ++ acc1, acc2 ++ [a])
            nacc2_delete a = delete a acc2
        in  case ele of
              Set a _ -> cond_add a
              Get a -> ([ele] ++ acc1, nacc2_delete a)
  in  foldr folder ([], []) [Set 5 1, Set 1 1, Set 1 2, Get 2, Get 1, Get 3, Set 1 3]

{-
fails on:
set 1 1
cond [ set 1 2 ]
get 1
-}
