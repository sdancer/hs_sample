module Lifter where

import Ast
import X86Sem
import Hapstone.Capstone
import Hapstone.Internal.Capstone as Capstone
import Hapstone.Internal.X86      as X86
import Util
import Data.Word
import BitVector

--x86 vs arm, etc?
mmp :: [CsMode] -> CsInsn -> Stmt Int

mmp modes a = Compound (fromIntegral (address a)) ((case toEnum (fromIntegral (insnId a)) of
  X86InsAdd -> add_s
  X86InsMov -> mov_s
  X86InsMovzx -> movzx_s
  X86InsSub -> sub_s
  X86InsCmp -> cmp_s
  X86InsPush -> push_s
  X86InsPop -> pop_s
  X86InsXor -> xor_s
  X86InsAnd -> and_s
  X86InsOr -> or_s
  X86InsJmp -> jmp_s
  X86InsJe -> je_s
  X86InsJne -> jne_s
  X86InsLea -> lea_s
  X86InsInc -> inc_s
  X86InsCall -> call_s
  X86InsRet -> ret_s
  X86InsTest -> test_s
  _ -> \_ _ -> [Comment ("Instruction " ++ mnemonic a ++ " not supported. Ignoring opcode.")]) modes a)

liftAsm :: [CsMode] -> [CsInsn] -> [Stmt Int]

liftAsm modes buf = map (mmp modes) buf

disasm_buf :: [CsMode] -> [Word8] -> IO (Either CsErr [CsInsn])

disasm_buf modes buffer = disasmSimpleIO $ disasm modes buffer 0

liftX86toAst :: [CsMode] -> [Word8] -> IO [Stmt Int]

liftX86toAst modes input = do
  asm <- disasm_buf modes input
  return (case asm of
    Left _ -> error "Error in disassembling machine code."
    Right b -> liftAsm modes b)
    
