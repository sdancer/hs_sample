module Lifter where

import            Ast
import            X86Sem
import            Hapstone.Capstone
import            Hapstone.Internal.Capstone as Capstone
import            Hapstone.Internal.X86      as X86
import            Util
import            Data.Word

mmp :: CsInsn -> [AstNodeType]
mmp a = case (mnemonic a) of
          "add" -> add_s a
          "mov" -> mov a
          otherwise -> [AssertNode otherwise]

liftAsm :: [CsInsn] -> [[AstNodeType]]
liftAsm buf = map mmp buf

disasm_buf :: [Word8] -> IO (Either CsErr [CsInsn])
disasm_buf buffer = disasmSimpleIO $ disasm buffer 0
