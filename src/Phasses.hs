module Phasses where

import Data.List

data SG = Set Int | Get Int deriving (Show, Eq)

naive_dead_code_elimination =
  let folder ele (acc1, acc2) =
        let cond_add = case (elem ele acc2) of
                      True -> (acc1, acc2) -- skipped
                      False -> (acc1 ++ [ele], acc2 ++ [ele])
            nacc2_delete = delete ele acc2
        in  case ele of
              Set a -> cond_add
              Get a -> ([ele] ++ acc1, nacc2_delete)
  in  foldr folder ([], []) [Set 1, Get 2, Get 3, Set 1]
