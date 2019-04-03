module Lifter where

import Ast
import X86Sem
import Hapstone.Capstone
import Hapstone.Internal.Capstone as Capstone
import Hapstone.Internal.X86      as X86
import Util
import Data.Word
import BitVector

import qualified Data.ByteString as BS
import Data.Int

-- should be on x86 sems file?
mmp :: [CsMode] -> CsInsn -> (Int, [Stmt])
mmp modes a = (convert (address a), (case toEnum (fromIntegral (insnId a)) of
  X86InsAdd -> add_s
  X86InsMov -> mov_s
  X86InsMovzx -> mov_s --will this trigger bits missaling?
  X86InsSub -> sub_s
  X86InsCmp -> cmp_s
  X86InsPush -> push_s
  X86InsPop -> pop_s
  X86InsXor -> xor_s
  X86InsAnd -> and_s
  X86InsOr -> or_s
  X86InsJmp -> jmp_s
  X86InsJe -> je_s
  X86InsLea -> lea_s
  X86InsInc -> inc_s
  otherwise -> error ("Instruction " ++ mnemonic a ++ " not supported.")) modes a)

liftAsm :: [CsMode] -> [CsInsn] -> [(Int, [Stmt])]
liftAsm modes buf = map (mmp modes) buf

disasm_buf :: [CsMode] -> Word64 -> [Word8] -> IO (Either CsErr [CsInsn])
disasm_buf modes addr buffer = disasmSimpleIO $ disasm modes buffer addr

liftX86toAst :: [CsMode] -> [Word8] -> IO [(Int, [Stmt])]
liftX86toAst modes input = do
    asm <- disasm_buf modes 0 input
    return (case asm of
      Left _ -> error "Error in disassembling machine code."
      Right b -> liftAsm modes b)


-- make generic for modes and etc
procqueue :: BS.ByteString -> [Word64] -> [(Word64, [Stmt], [Word64], [Word64])] -> IO ([(Word64,[Stmt])])
procqueue filedata [] acc =
  summarize blocks
procqueue filedata (start_addr:queue) acc =
  --Xrefs from
  --Xrefs to
  let one_inst address = BS.unpack $ BS.take 16 (BS.drop address filedata)
      liftone asm = case asm of
                      Left _ -> []
                      Right b -> liftAsm [Capstone.CsMode32] [(head b)]
  asm <- disasm_buf [Capstone.CsMode32] start_addr (one_inst $ fromIntegral start_addr)
  another <- liftone asm
  let newacc = case another of
    [] -> acc
    (addr, stm) -> do
        let xrefs_to = getxrefs_to stm
        in  (acc ++ [(addr, stm, xrefs_to, [])])
  procqueue filedata queue newacc



liftblocks :: BS.ByteString -> Word64 -> IO ([(Int,[Stmt])])
liftblocks filedata start_addr = do
  procqueue filedata [start_addr] []
--
-- cfg lifter:
--      add initial address to queue
--
--      proc queue:
--       read/proc instruction
--
--      for each address:
--       (address, refs_to)
--
--      when queue is empty:
--       coagulate direct refs_to:1 -> == refs_from:1 into blocks
--
