module AstContext where

import Ast

import Hapstone.Capstone
import Hapstone.Internal.Capstone as Capstone
import Hapstone.Internal.X86      as X86
import Data.Word
import Util

getOperandAst :: [CsMode] -> CsX86Op -> Expr
getOperandAst modes op = case value op of
  (Imm value) -> BvExpr (convert value) (convert (size op) * 8)
  (Reg reg) -> GetReg (compoundReg reg)
  (Mem mem) -> Load (convert $ size op) (getLeaAst modes mem)

--getZxRegister :: CsMode -> CompoundRegister

getLeaAst :: [CsMode] -> X86OpMemStruct -> Expr
getLeaAst modes mem =
  (BvaddExpr node_disp (BvaddExpr node_base node_index)) where
    arch_size = get_arch_size modes
    node_base = case base mem of
      X86RegInvalid -> (BvExpr 0 arch_size)
      reg -> GetReg (compoundReg reg)
    node_index = case index mem of
      X86RegInvalid -> (BvExpr 0 arch_size)
      reg -> BvmulExpr (GetReg (compoundReg reg)) (BvExpr (fromIntegral $ scale mem) arch_size)
    node_disp = BvExpr (fromIntegral $ disp' mem) arch_size

