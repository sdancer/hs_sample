module Lifter where

import            Ast
import            X86Sem
import            Hapstone.Capstone
import            Hapstone.Internal.Capstone as Capstone
import            Hapstone.Internal.X86      as X86
import            Util
import            Data.Word

--x86 vs arm, etc?
mmp :: [CsMode] -> CsInsn -> [AstNodeType]
mmp modes a = case toEnum (fromIntegral (insnId a)) of
  X86InsAdd -> add_s a
  X86InsMov -> mov a
  X86InsSub -> sub_s a
  X86InsPush -> push_s a
  X86InsPop -> pop_s a
  X86InsXor -> xor_s a
  otherwise -> [AssertNode (mnemonic a)]

liftAsm :: [CsMode] -> [CsInsn] -> [[AstNodeType]]
liftAsm modes buf = map (mmp modes) buf

disasm_buf :: [CsMode] -> [Word8] -> IO (Either CsErr [CsInsn])
disasm_buf modes buffer = disasmSimpleIO $ disasm modes buffer 0

liftX86toAst :: [CsMode] -> [Word8] -> IO [[AstNodeType]]
liftX86toAst modes input = do
    asm <- disasm_buf modes input
    return (case asm of
      Left _ -> [[(AssertNode "dissasm error")]]
      Right b -> liftAsm modes b)
