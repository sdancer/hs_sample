module AstContext where

import Ast

import           Hapstone.Capstone
import           Hapstone.Internal.Capstone as Capstone
import           Hapstone.Internal.X86      as X86
import           Data.Word

getOperandAst :: CsX86OpValue -> AstNodeType
getOperandAst (Imm value) = BvNode value 32
getOperandAst (Reg reg) = (GetReg (X86Reg reg))
getOperandAst (Mem mem) = Read node_lea
    where
      -- compose lea as
      node_lea = (BvaddNode node_disp (BvaddNode node_base node_index) )
      node_base = case base mem of
          X86RegInvalid -> (BvNode 0 32)
          reg -> GetReg (X86Reg reg)
      node_index = case index mem of
          X86RegInvalid -> (BvNode 0 32)
          reg ->
            BvmulNode (GetReg (X86Reg reg)) (BvNode (fromIntegral $ scale mem) 32)
      node_disp = BvNode (fromIntegral $ disp' mem) 32

      -- mem_index = if index mem == X86RegInvalid then 0 else (get_int_value (index mem) state) * mem_scale
      -- mem_scale = fromIntegral(scale mem)::Word64
      -- mem_disp = fromIntegral(disp' mem)::Word64
