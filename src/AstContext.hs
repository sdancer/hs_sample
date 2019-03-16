module AstContext where

import Ast

import           Hapstone.Capstone
import           Hapstone.Internal.Capstone as Capstone
import           Hapstone.Internal.X86      as X86
import           Data.Word

getOperandAst :: CsX86Op -> AstNode
getOperandAst op = case value op of
  (Imm value) -> BvNode value ((size op) * 8)
  (Reg reg) -> GetReg (X86Reg reg)
  (Mem mem) -> Read (getLeaAst mem)


getLeaAst :: X86OpMemStruct -> AstNode
getLeaAst mem =
    (BvaddNode node_disp (BvaddNode node_base node_index) ) where
        node_base = case base mem of
            X86RegInvalid -> (BvNode 0 32)
            reg -> GetReg (X86Reg reg)
        node_index = case index mem of
            X86RegInvalid -> (BvNode 0 32)
            reg ->
              BvmulNode (GetReg (X86Reg reg)) (BvNode (fromIntegral $ scale mem) 32)
        node_disp = BvNode (fromIntegral $ disp' mem) 32
