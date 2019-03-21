module AstContext where

import Ast

import Hapstone.Capstone
import Hapstone.Internal.Capstone as Capstone
import Hapstone.Internal.X86      as X86
import Data.Word
import Util

getOperandAst :: CsX86Op -> Expr
getOperandAst op = case value op of
  (Imm value) -> BvExpr (convert value) (convert (size op) * 8)
  (Reg reg) -> GetReg (compoundReg reg)
  (Mem mem) -> Load (convert $ size op) (getLeaAst mem)


getLeaAst :: X86OpMemStruct -> Expr
getLeaAst mem =
  (BvaddExpr node_disp (BvaddExpr node_base node_index) ) where
    node_base = case base mem of
      X86RegInvalid -> (BvExpr 0 32)
      reg -> GetReg (compoundReg reg)
    node_index = case index mem of
      X86RegInvalid -> (BvExpr 0 32)
      reg -> BvmulExpr (GetReg (compoundReg reg)) (BvExpr (fromIntegral $ scale mem) 32)
    node_disp = BvExpr (fromIntegral $ disp' mem) 32

