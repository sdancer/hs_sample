module StackTest where

import Data.List
import Data.Word
--import Data.ByteString

-- push eax
-- pop ebx
ex1 = [0x50, 0x5B]

-- push eax
-- mov eax, 1
-- pop eax
ex2 = [0x50, 0xB8, 0x01, 0x00, 0x00, 0x00, 0x58]

-- push eax
-- mov eax, 1
-- mov ebx, eax
-- pop eax
ex3  = [0x50, 0xB8, 0x01, 0x00, 0x00, 0x00, 0x89, 0xC3, 0x58]

-- sym_initial_reg()
-- stack -- -> grow stack scope
-- stack ++ -> delete refs scope

data Valtype = Concrete Word8
               | Sym Int Int deriving (Show, Eq)
