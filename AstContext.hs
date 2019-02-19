module AstContext where

import Ast

import           Hapstone.Capstone
import           Hapstone.Internal.Capstone as Capstone
import           Hapstone.Internal.X86      as X86

getOperandAst :: CsX86OpValue -> AstNodeType
getOperandAst (Imm value) = BvNode value 32
getOperandAst (Reg reg) = BvNode 0 32
getOperandAst (Mem mem) = Read $ BvNode 0 32
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
