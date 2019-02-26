module Test1 where

{- |
mov eax, 1
mov eax, 2

first pass(individual instruction lift):
(setreg 'eax' (lit 1))
(setreg 'eax' (lit 2))

after simplification:
(setreg 'eax' (lit 2))
-}
