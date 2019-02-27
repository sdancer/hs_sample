module Util where

import qualified Data.ByteString            as BS
import qualified Data.List                  as List
import           Data.Word
import           Hapstone.Capstone
import           Hapstone.Internal.Capstone as Capstone
import           Hapstone.Internal.X86      as X86
import           Numeric                    (showHex)

getBinPart :: BS.ByteString -> Int -> Int -> [Word8]
getBinPart contents from cnt = BS.unpack $ BS.take cnt $ BS.drop from contents

disasm :: [Word8] -> Word64 -> Disassembler ()
disasm intel_asm_buf start_addr = Disassembler {
    arch = Capstone.CsArchX86 -- ^ Options: CsArchArm, CsArchArm64, CsArchMips, CsArchX86, CsArchPpc, CsArchSparc, CsArchSysz, CsArchXcore
    , modes = [Capstone.CsMode32] -- ^ Modes (some may be combined by adding to the list): CsModeLittleEndian, CsModeArm, CsMode16 (16-bit x86), CsMode32 (32-bit x86), CsMode64 (64-bit x86-64/amd64 or PPC), CsModeThumb, CsModeMclass, CsModeV8 (ARMv8 A32), CsModeMicro, CsModeMips3, CsModeMips32r6, CsModeMipsGp64, CsModeV9 (SparcV9 mode), CsModeBigEndian, CsModeMips32, CsModeMips64
    , buffer = intel_asm_buf -- ^ buffer to disassemble, as [Word8]
    , addr = start_addr -- ^ address of first byte in the buffer, as Word64
    , num = 0 -- ^ number of instructions to disassemble (0 for maximum)
    , Hapstone.Capstone.detail = True -- ^ include detailed information? True/False
    , Hapstone.Capstone.skip = Just (defaultSkipdataStruct) -- ^ setup SKIPDATA options, as Maybe CsSkipdataStruct
    , action = defaultAction
    }

-- Convert a instruction to string
insn_to_str :: CsInsn -> [Char]
insn_to_str insn = "0x" ++ a ++ ":\t" ++ m ++ "\t" ++ o
    where m = mnemonic insn
          o = opStr insn
          a = (showHex $ address insn) ""

-- return first operand in a instruction
insn_opr :: Int -> CsInsn -> Maybe CsX86Op
insn_opr i insn = case Capstone.detail insn of
  Nothing -> Nothing
  Just d -> case archInfo d of
    Nothing        -> Nothing
    Just (X86 ari) -> if length (operands ari) < i
      then Nothing
      else Just $ (operands ari) !! i

get_first_opr_value :: CsInsn -> CsX86OpValue
get_first_opr_value insn = case insn_opr 0 insn of
  Nothing -> error ("nothing in first operand " ++ show insn)
  Just op -> value op

get_second_opr_value :: CsInsn -> CsX86OpValue
get_second_opr_value insn = case insn_opr 1 insn of
  Nothing -> error "nothing in second operand"
  Just op -> value op

-- calc next instruction address
next_addr :: CsInsn -> Word64
next_addr insn = (address insn) + insn_size
  where
    insn_size = fromIntegral(length $ bytes insn)::Word64

-- check if an instruction has a group (call/ret/jump/...)
contains_group :: X86InsnGroup -> CsInsn -> Bool
contains_group gr insn = case Capstone.detail insn of
  Nothing -> False
  Just d  -> do
    let grw8 = fromIntegral(fromEnum gr) :: Word8
    List.elem grw8 $ groups d
