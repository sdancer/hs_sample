module Lifter where

import            Ast
import            X86Sem
import            Hapstone.Capstone
import            Hapstone.Internal.Capstone as Capstone
import            Hapstone.Internal.X86      as X86
import            Util
import            Data.Word

--x86 vs arm, etc?
mmp :: CsInsn -> [AstNodeType]
mmp a = case (mnemonic a) of
          "add" -> add_s a
          "sub" -> sub_s a
          "mov" -> mov a
          "xor" -> xor_s a
          "push" -> push a
          "pop" -> pop a
          otherwise -> [AssertNode otherwise]

liftAsm :: [CsInsn] -> [[AstNodeType]]
liftAsm buf = map mmp buf

disasm_buf :: [Word8] -> IO (Either CsErr [CsInsn])
disasm_buf buffer = disasmSimpleIO $ disasm buffer 0

liftX86toAst :: [Word8] -> IO [[AstNodeType]]
liftX86toAst input = do
    asm <- disasm_buf input
    return (case asm of
      Left _ -> [[(AssertNode "dissasm error")]]
      Right b -> liftAsm b)
