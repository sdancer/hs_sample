module Lifter where

import Ast
import X86Sem
import Hapstone.Capstone
import Hapstone.Internal.Capstone as Capstone
import Hapstone.Internal.X86      as X86
import Util
import Data.Word

labelStmts :: CsInsn -> [Stmt] -> [(Label, Stmt)]
labelStmts insn stmts =
  let addr = convert $ address insn
    in zip (zip [addr,addr..] [0,1..]) stmts

--x86 vs arm, etc?
mmp :: [CsMode] -> CsInsn -> [(Label, Stmt)]
mmp modes a = labelStmts a (case toEnum (fromIntegral (insnId a)) of
  X86InsAdd -> add_s a
  X86InsMov -> mov_s a
  X86InsSub -> sub_s a
  X86InsPush -> push_s modes a
  X86InsPop -> pop_s modes a
  X86InsXor -> xor_s a
  X86InsAnd -> and_s a
  X86InsOr -> or_s a
  X86InsJmp -> jmp_s a
  otherwise -> error ("Instruction " ++ mnemonic a ++ " not supported."))

liftAsm :: [CsMode] -> [CsInsn] -> [[(Label, Stmt)]]
liftAsm modes buf = map (mmp modes) buf

disasm_buf :: [CsMode] -> [Word8] -> IO (Either CsErr [CsInsn])
disasm_buf modes buffer = disasmSimpleIO $ disasm modes buffer 0

liftX86toAst :: [CsMode] -> [Word8] -> IO [[(Label, Stmt)]]
liftX86toAst modes input = do
    asm <- disasm_buf modes input
    return (case asm of
      Left _ -> error "Error in disassembling machine code."
      Right b -> liftAsm modes b)

