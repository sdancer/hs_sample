module AstContext where

import Ast

import           Hapstone.Capstone
import           Hapstone.Internal.Capstone as Capstone
import           Hapstone.Internal.X86      as X86
import           Data.Word

getOperandAst :: CsX86OpValue -> AstNodeType
getOperandAst (Imm value) = BvNode value 32
getOperandAst (Reg reg) = BvNode 0 32
getOperandAst (Mem mem) = Read node_lea
    where
      -- compose lea as
      node_lea = (BvaddNode (BvaddNode node_base node_index) node_disp)
      node_base = case base mem of
          X86RegInvalid -> (BvNode 0 32)
          reg -> (GetReg (X86Reg reg))
      node_index = case index mem of
          X86RegInvalid -> (BvNode 0 32)
          reg -> (GetReg (X86Reg reg))
      --node_scale = if scale mem
      node_disp = (BvNode (fromIntegral (disp' mem)) 32)

      -- mem_index = if index mem == X86RegInvalid then 0 else (get_int_value (index mem) state) * mem_scale
      -- mem_scale = fromIntegral(scale mem)::Word64
      -- mem_disp = fromIntegral(disp' mem)::Word64


-- a lea expression
-- if is_valid_stack_ref mem state
--     then do
--       let offset = stack_offset mem state
--       let current_stack_pos = esp $ regs state
--       let stack_map = stack state
--       case fetch_reg_contents X86RegEsp state of
--         NumVal current_stack_pos -> if List.elem (current_stack_pos + offset) $ keys stack_map
--           then stack_map ! (current_stack_pos + offset)
--           else InitStackVal (current_stack_pos + offset - initial_stack_pos)
--         other -> other
--     else error "bad mem type"
