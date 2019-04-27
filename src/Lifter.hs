module Lifter where

import Ast
import X86Sem
import Hapstone.Capstone
import Hapstone.Internal.Capstone as Capstone
import Hapstone.Internal.X86      as X86
import Util
import Data.Word
import BitVector
import SymbolicEval
import Phasses
import Data.Maybe

--x86 vs arm, etc?
mmp :: [CsMode] -> CsInsn -> IdStmt

mmp modes a = (case toEnum (fromIntegral (insnId a)) of
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
  _ -> \_ _ -> Comment (fromIntegral (address a)) ("Instruction " ++ mnemonic a ++ " not supported. Ignoring opcode.")) modes a

liftAsm :: [CsMode] -> [CsInsn] -> [IdStmt]

liftAsm modes buf = map (mmp modes) buf

disasm_buf :: [CsMode] -> [Word8] -> IO (Either CsErr [CsInsn])

disasm_buf modes buffer = disasmSimpleIO $ disasm modes buffer 0

liftX86toAst :: [CsMode] -> [Word8] -> IO [IdStmt]

liftX86toAst modes input = do
  asm <- disasm_buf modes input
  return (case asm of
    Left _ -> error "Error in disassembling machine code."
    Right b -> liftAsm modes b)

-- Takes the processor mode, writable address ranges, and executable code. Validates
-- memory accesses and returns a simplified Stmt in the IR that is semantically
-- equivalent.

decompile :: [CsMode] -> [(Expr, Expr)] -> [Word8] -> IO IdStmt

decompile modes writableMemory input = do
  asm <- disasm_buf modes input
  let lifted = case asm of
                  Left _ -> error "error on disasm"
                  Right b -> liftAsm modes b
  -- Label the statements produced by lifting from assembly. The labels are necessary for
  -- the cross referencing that happens in the next stage.
  let labelled = snd $ labelStmts 0 (Compound undefined $ lifted)
  -- Simplify the labelled statements by doing constant propagation and folding.
  simplified <- symExec (symExecContext modes) labelled
  -- The program is only able to modify addresses that can be proven to be in writableMemory
  validateWrites writableMemory (snd simplified)
  -- Eliminate the dead SetRegs under the assumption that the flag bits are defined-before-use
  -- in the fragment of code that follows simplified. Wrap it in a statement.
  let srEliminated = Compound (-1) $ maybeToList $ snd $ eliminateDeadSetRegs [(1408,1472)] (snd simplified)
  -- Eliminate the dead Stores. Wrap it in a statement.
  sEliminated <- Compound (-1) <$> maybeToList <$> snd <$> eliminateDeadStores [] srEliminated
  -- Now introduce cross references into the statements. This must be done after dead code
  -- elimination as it obscures the locations where expressions are loaded from storage.
  referenced <- insertRefs (symExecContext modes) sEliminated
  -- Now return the result of the above transformations.
  return (absToIdStmt $ snd referenced)

