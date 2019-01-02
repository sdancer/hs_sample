{-|
Description : X86 Example using Hapstone
Copyright   : (c) Garret Wassermann, 2017
License     : BSD3
Maintainer  : Garret Wassermann <gwasser@gmail.com>
Stability   : experimental

This is example code that shows how to use the Hapstone bindings,
based on an X86 example provided with the python bindings to Capstone.
For more information, see http://www.capstone-engine.org/lang_python.html.
-}
module Main
where

import GHC.Int
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Binary.Get ( isEmpty, runGet )
import qualified Data.Binary as B

import Data.Word
import Numeric (showHex)

import Hapstone.Capstone
import Hapstone.Internal.Capstone as Capstone

myAction2 :: Capstone.CsInsn -> IO ()
myAction2 insn = do
    let m = mnemonic insn
    case m of
      "add" -> print "got a jump!"
      _ -> do
        Prelude.putStrLn ("0x" ++ a ++ ":\t" ++ m ++ "\t" ++ o)
        where --m = mnemonic insn
              o = opStr insn
              a = (showHex $ address insn) ""

lift_block :: BSL.ByteString -> GHC.Int.Int64 -> IO [()]
lift_block bin_data start_address = do
    let x1 = BSL.drop (start_address-0x400000) bin_data
    let x2 = BSL.take 1024 x1
    let x3 = BSL.unpack x2
    --print (showHex 0 x3)
    -- printChars $ runGet getChars contents

    x <- disasmIO (mydis x3)
    case x of
      Left error ->
                return []
      Right inst -> do
          as <- mapM (myAction2) (map takeinst inst)
          _ <- print "ok, what now?"
          return as
    where
      mydis ibuffer = Disassembler {
        arch = Capstone.CsArchX86 -- ^ Options: CsArchArm, CsArchArm64, CsArchMips, CsArchX86, CsArchPpc, CsArchSparc, CsArchSysz, CsArchXcore
        , modes = [Capstone.CsMode32] -- ^ Modes (some may be combined by adding to the list): CsModeLittleEndian, CsModeArm, CsMode16 (16-bit x86), CsMode32 (32-bit x86), CsMode64 (64-bit x86-64/amd64 or PPC), CsModeThumb, CsModeMclass, CsModeV8 (ARMv8 A32), CsModeMicro, CsModeMips3, CsModeMips32r6, CsModeMipsGp64, CsModeV9 (SparcV9 mode), CsModeBigEndian, CsModeMips32, CsModeMips64
        , buffer = ibuffer -- ^ buffer to disassemble, as [Word8]
        , addr = 0x1000 -- ^ address of first byte in the buffer, as Word64
        , num = 0 -- ^ number of instructions to disassemble (0 for maximum)
        , Hapstone.Capstone.detail = True -- ^ include detailed information? True/False
        , skip = Just (defaultSkipdataStruct) -- ^ setup SKIPDATA options, as Maybe CsSkipdataStruct
        , action = defaultAction -- ^ action to run on each instruction, a function with signature Csh -> CsInsn -> IO a; default is defaultAction
        }


type Chars = [B.Word8]


takeinst :: (Capstone.CsInsn, a) -> Capstone.CsInsn
takeinst (inst, a) = inst

-- disasmIO has signature Disassembler a -> IO (Either CsErr [(CsInsn, a)])
main = do
  contents2 <- BSL.readFile "blackcipher.aes"
  lift_block contents2 0x98717E
