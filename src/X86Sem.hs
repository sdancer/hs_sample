module X86Sem where

import Ast
import Hapstone.Capstone
import Hapstone.Internal.Capstone as Capstone
import Hapstone.Internal.X86      as X86
import Util
import AstContext
import Data.Maybe
import Data.Word
import BitVector

getCsX86arch :: Maybe CsDetail -> Maybe CsX86

getCsX86arch inst =
  let arch = maybe Nothing archInfo inst
  in case arch of
    Just (X86 csx86) -> Just csx86
    _ -> Nothing

x86operands :: CsInsn -> [CsX86Op]

x86operands inst =
  let arch2 = getCsX86arch (Capstone.detail inst)
      ops = maybe [] operands arch2
  in ops

-- Makes an expression representing the address of the next instruction

nextInstrPtr :: [CsMode] -> CsInsn -> Expr

nextInstrPtr modes insn = BvExpr (toBv (insnAddr + insnLen) archBitSize)
  where insnAddr = fromIntegral (address insn)
        insnLen = fromIntegral (length (bytes insn))
        archBitSize = fromIntegral (getArchBitSize modes)

-- Make operation to increase instruction pointer by size of instruction

incInsnPtr :: [CsMode] -> CsInsn -> IdStmt

incInsnPtr modes insn = SetReg undefined (getInsnPtr modes) (nextInstrPtr modes insn)

-- Gets an expression for the given operand. Processor mode may be needed for computing
-- effective addresses.

getOperandAst :: [CsMode] -> CsX86Op -> Expr

getOperandAst modes op =
  let opBitSize = fromIntegral (size op) * byteSizeBit
  in case value op of
    (Imm value) -> BvExpr (toBv value opBitSize)
    (Reg reg) -> GetReg (fromX86Reg reg)
    (Mem mem) -> Load opBitSize (getLeaAst modes mem)

-- Gets the specified register after it has been zero extended to the architecture size

getZxRegister :: [CsMode] -> CompoundReg -> Expr

getZxRegister modes reg = ZxExpr (getArchBitSize modes) (GetReg reg)

-- Gets an expression for the memory location of the given memory operand

getLeaAst :: [CsMode] -> X86OpMemStruct -> Expr

getLeaAst modes mem =
  (BvaddExpr nodeDisp (BvaddExpr nodeBase nodeIndex)) where
    archBitSize = getArchBitSize modes
    nodeBase = case base mem of
      X86RegInvalid -> BvExpr (toBv 0 archBitSize)
      reg -> getZxRegister modes (fromX86Reg reg)
    nodeIndex = case index mem of
      X86RegInvalid -> BvExpr (toBv 0 archBitSize)
      reg -> BvmulExpr (getZxRegister modes (fromX86Reg reg)) (BvExpr (toBv (scale mem) archBitSize))
    nodeDisp = BvExpr (toBv (disp' mem) archBitSize)

-- Make operation to store the given expression in the given operand

storeStmt :: [CsMode] -> CsX86Op -> Expr -> IdStmt

storeStmt modes operand storeWhat =
  case (value operand) of
    (Reg reg) -> SetReg undefined (fromX86Reg reg) storeWhat
    (Mem mem) -> Store undefined (getLeaAst modes mem) storeWhat
    _ -> Comment undefined ("Target of store operation is neither a register nor a memory operand.")

-- Make operation to set the zero flag to the value that it would have after some operation

zfSem :: Expr -> CsX86Op -> IdStmt

zfSem parent dst =
  let bvSize = (fromIntegral $ size dst) * 8 in
    SetReg undefined (fromX86Flag X86FlagZf) (IteExpr
      (EqualExpr (ExtractExpr 0 bvSize parent) (BvExpr (toBv 0 bvSize)))
      (BvExpr (toBv 1 1))
      (BvExpr (toBv 0 1)))

-- Make operation to set the overflow flag to the value that it would have after an add operation

ofAddSem :: Expr -> CsX86Op -> Expr -> Expr -> IdStmt

ofAddSem parent dst op1ast op2ast =
  let bvSize = (fromIntegral $ size dst) * 8 in
    SetReg undefined (fromX86Flag X86FlagOf) (ExtractExpr (bvSize - 1) bvSize
      (BvandExpr
        (BvxorExpr op1ast (BvnotExpr op2ast))
        (BvxorExpr op1ast (ExtractExpr 0 bvSize parent))))

-- Make operation to set the carry flag to the value that it would have after an add operation

cfAddSem :: Expr -> CsX86Op -> Expr -> Expr -> IdStmt

cfAddSem parent dst op1ast op2ast =
  let bvSize = (fromIntegral $ size dst) * 8 in
    SetReg undefined (fromX86Flag X86FlagCf) (ExtractExpr (bvSize - 1) bvSize
      (BvxorExpr (BvandExpr op1ast op2ast)
        (BvandExpr (BvxorExpr
            (BvxorExpr op1ast op2ast)
            (ExtractExpr 0 bvSize parent))
          (BvxorExpr op1ast op2ast))))

-- Make operation to set the adjust flag to the value that it would have after some operation

afSem :: Expr -> CsX86Op -> Expr -> Expr -> IdStmt

afSem parent dst op1ast op2ast =
  let bvSize = (fromIntegral $ size dst) * 8 in
    SetReg undefined (fromX86Flag X86FlagAf) (IteExpr
      (EqualExpr
        (BvExpr (toBv 0x10 bvSize))
        (BvandExpr
          (BvExpr (toBv 0x10 bvSize))
          (BvxorExpr
            (ExtractExpr 0 bvSize parent)
            (BvxorExpr op1ast op2ast))))
      (BvExpr (toBv 1 1))
      (BvExpr (toBv 0 1)))

-- Make operation to set the parity flag to the value that it would have after some operation

pfSem :: Expr -> CsX86Op -> IdStmt

pfSem parent dst =
  let loop counter =
        (if counter == byteSizeBit
          then BvExpr (toBv 1 1)
          else (BvxorExpr
            (loop (counter + 1))
            (ExtractExpr 0 1
              (BvlshrExpr
                (ExtractExpr 0 byteSizeBit parent)
                (BvExpr (toBv counter byteSizeBit)))))) in
    SetReg undefined (fromX86Flag X86FlagPf) (loop 0)

-- Make operation to set the sign flag to the value that it would have after some operation

sfSem :: Expr -> CsX86Op -> IdStmt

sfSem parent dst =
  let bvSize = (fromIntegral $ size dst) * 8 in
    SetReg undefined (fromX86Flag X86FlagSf) (ExtractExpr (bvSize - 1) bvSize parent)

-- Make operation to set the carry flag to the value that it would have after an sub operation

cfSubSem :: Expr -> CsX86Op -> Expr -> Expr -> IdStmt

cfSubSem parent dst op1ast op2ast =
  let bvSize = (fromIntegral $ size dst) * 8 in
    SetReg undefined (fromX86Flag X86FlagCf) (ExtractExpr (bvSize - 1) bvSize
      (BvxorExpr
        (BvxorExpr op1ast (BvxorExpr op2ast (ExtractExpr 0 bvSize parent)))
        (BvandExpr
          (BvxorExpr op1ast (ExtractExpr 0 bvSize parent))
          (BvxorExpr op1ast op2ast))))

-- Make operation to set the overflow flag to the value that it would have after an sub operation

ofSubSem :: Expr -> CsX86Op -> Expr -> Expr -> IdStmt

ofSubSem parent dst op1ast op2ast =
  let bvSize = (fromIntegral $ size dst) * 8 in
    SetReg undefined (fromX86Flag X86FlagOf) (ExtractExpr (bvSize - 1) bvSize
      (BvandExpr
        (BvxorExpr op1ast op2ast)
        (BvxorExpr op1ast (ExtractExpr 0 bvSize parent))))

getInsnId :: CsInsn -> X86Insn

getInsnId a = toEnum (fromIntegral (insnId a))

-- Makes a singleton list containing the argument if the condition is true. Otherwise makes
-- the empty list.

includeIf :: Bool -> [a] -> [a]

includeIf cond sublist = if cond then sublist else []

-- Creates a statement in the IR with semantics equivalent to those of the given
-- instruction when interpreted in the given mode.

liftX86 :: [CsMode] -> CsInsn -> IdStmt

-- ADD instruction

liftX86 modes inst | getInsnId inst == X86InsAdd =
  let (dst : src : _ ) = x86operands inst
      dstAst = getOperandAst modes dst
      srcAst = getOperandAst modes src
      addNode = (BvaddExpr dstAst srcAst)
  in Compound (fromIntegral (address inst)) [
      incInsnPtr modes inst,
      storeStmt modes dst addNode,
      afSem addNode dst dstAst srcAst,
      pfSem addNode dst,
      sfSem addNode dst,
      zfSem addNode dst,
      cfAddSem addNode dst dstAst srcAst,
      ofAddSem addNode dst dstAst srcAst]

-- INC instruction

liftX86 modes inst | getInsnId inst == X86InsInc =
  let (dst : _ ) = x86operands inst
      dstAst = getOperandAst modes dst
      srcAst = BvExpr (toBv 1 (fromIntegral (size dst * byteSizeBit)))
      addNode = (BvaddExpr dstAst srcAst)
  in Compound (fromIntegral (address inst)) [
      incInsnPtr modes inst,
      storeStmt modes dst addNode,
      afSem addNode dst dstAst srcAst,
      pfSem addNode dst,
      sfSem addNode dst,
      zfSem addNode dst,
      ofAddSem addNode dst dstAst srcAst]

-- SUB instruction

liftX86 modes inst | getInsnId inst == X86InsSub =
  let (dst : src : _ ) = x86operands inst
      dstAst = getOperandAst modes dst
      srcAst = getOperandAst modes src
      subNode = (BvsubExpr dstAst srcAst)
  in Compound (fromIntegral (address inst)) [
      incInsnPtr modes inst,
      storeStmt modes dst subNode,
      afSem subNode dst dstAst srcAst,
      pfSem subNode dst,
      sfSem subNode dst,
      zfSem subNode dst,
      cfSubSem subNode dst dstAst srcAst,
      ofSubSem subNode dst dstAst srcAst]

-- CMP instruction

liftX86 modes inst | getInsnId inst == X86InsCmp =
  let (dst : src : _ ) = x86operands inst
      dstAst = getOperandAst modes dst
      srcAst = SxExpr (fromIntegral (size dst) * byteSizeBit) (getOperandAst modes src)
      cmpNode = BvsubExpr dstAst srcAst
  in Compound (fromIntegral (address inst)) [
      incInsnPtr modes inst,
      afSem cmpNode dst dstAst srcAst,
      pfSem cmpNode dst,
      sfSem cmpNode dst,
      zfSem cmpNode dst,
      cfSubSem cmpNode dst dstAst srcAst,
      ofSubSem cmpNode dst dstAst srcAst]

-- XOR instruction

liftX86 modes inst | getInsnId inst == X86InsXor =
  let (dstOp : srcOp : _ ) = x86operands inst
      dstAst = getOperandAst modes dstOp
      srcAst = getOperandAst modes srcOp
      xorNode = (BvxorExpr dstAst srcAst)
  in Compound (fromIntegral (address inst)) [
      incInsnPtr modes inst,
      storeStmt modes dstOp xorNode,
      SetReg undefined (fromX86Flag X86FlagAf) (UndefinedExpr 1),
      pfSem xorNode dstOp,
      sfSem xorNode dstOp,
      zfSem xorNode dstOp,
      SetReg undefined (fromX86Flag X86FlagCf) (BvExpr (toBv 0 1)),
      SetReg undefined (fromX86Flag X86FlagOf) (BvExpr (toBv 0 1))]

-- AND instruction

liftX86 modes inst | getInsnId inst == X86InsAnd =
  let (dstOp : srcOp : _ ) = x86operands inst
      dstAst = getOperandAst modes dstOp
      srcAst = getOperandAst modes srcOp
      andNode = (BvandExpr dstAst srcAst)
  in Compound (fromIntegral (address inst)) [
      incInsnPtr modes inst,
      storeStmt modes dstOp andNode,
      SetReg undefined (fromX86Flag X86FlagAf) (UndefinedExpr 1),
      pfSem andNode dstOp,
      sfSem andNode dstOp,
      zfSem andNode dstOp,
      SetReg undefined (fromX86Flag X86FlagCf) (BvExpr (toBv 0 1)),
      SetReg undefined (fromX86Flag X86FlagOf) (BvExpr (toBv 0 1))]

-- TEST instruction

liftX86 modes inst | getInsnId inst == X86InsTest =
  let (dstOp : srcOp : _ ) = x86operands inst
      dstAst = getOperandAst modes dstOp
      srcAst = getOperandAst modes srcOp
      testNode = (BvandExpr dstAst srcAst)
  in Compound (fromIntegral (address inst)) [
      incInsnPtr modes inst,
      SetReg undefined (fromX86Flag X86FlagAf) (UndefinedExpr 1),
      pfSem testNode dstOp,
      sfSem testNode dstOp,
      zfSem testNode dstOp,
      SetReg undefined (fromX86Flag X86FlagCf) (BvExpr (toBv 0 1)),
      SetReg undefined (fromX86Flag X86FlagOf) (BvExpr (toBv 0 1))]

-- OR instruction

liftX86 modes inst | getInsnId inst == X86InsOr =
  let (dstOp : srcOp : _ ) = x86operands inst
      dstAst = getOperandAst modes dstOp
      srcAst = getOperandAst modes srcOp
      andNode = (BvorExpr dstAst srcAst)
  in Compound (fromIntegral (address inst)) [
      incInsnPtr modes inst,
      storeStmt modes dstOp andNode,
      SetReg undefined (fromX86Flag X86FlagAf) (UndefinedExpr 1),
      pfSem andNode dstOp,
      sfSem andNode dstOp,
      zfSem andNode dstOp,
      SetReg undefined (fromX86Flag X86FlagCf) (BvExpr (toBv 0 1)),
      SetReg undefined (fromX86Flag X86FlagOf) (BvExpr (toBv 0 1))]

-- PUSH instruction

liftX86 modes inst | getInsnId inst == X86InsPush =
  let (src : _) = x86operands inst
      sp = getStackReg modes
      archByteSize = getArchByteSize modes
      -- If it's an immediate source, the memory access is always based on the arch size
      opSize = case (value src) of
        (Imm _) -> archByteSize
        _ -> fromIntegral $ size src
  in Compound (fromIntegral (address inst)) [
      incInsnPtr modes inst,
      SetReg undefined sp (BvsubExpr (GetReg sp) (BvExpr (toBv opSize (archByteSize * byteSizeBit)))),
      Store undefined (GetReg sp) (ZxExpr (opSize * byteSizeBit) (getOperandAst modes src))]

-- POP instruction

liftX86 modes inst | getInsnId inst == X86InsPop =
  let (dst : _) = x86operands inst
      sp = getStackReg modes
      archBitSize = getArchBitSize modes
      opSize = fromIntegral $ size dst
      -- Is the ESP register is used as a base register for addressing a destination operand in memory?
      spBase = case (value dst) of
        (Mem memStruct) -> isSubregisterOf (fromX86Reg (base memStruct)) sp
        _ -> False
      -- Is the destination register is SP?
      spReg = case (value dst) of
        (Reg reg) -> isSubregisterOf (fromX86Reg reg) sp
        _ -> False
      -- An expression of the amount the stack pointer will be increased by
      deltaVal = BvExpr (toBv opSize archBitSize)
  in Compound (fromIntegral (address inst))
      ([incInsnPtr modes inst]
      ++ (includeIf spBase [SetReg undefined sp (BvaddExpr (GetReg sp) deltaVal)])
      ++ [storeStmt modes dst
          (Load (opSize * byteSizeBit)
            (if spBase then (BvsubExpr (GetReg sp) deltaVal) else (GetReg sp)))]
      ++ (includeIf (not (spBase || spReg)) [SetReg undefined sp (BvaddExpr (GetReg sp) deltaVal)]))

-- MOV instruction

liftX86 modes inst | getInsnId inst == X86InsMov =
  let (dstOp : srcOp : _ ) = x86operands inst
      dstAst = getOperandAst modes dstOp
      srcAst = getOperandAst modes srcOp
      dstSizeBit = (fromIntegral $ size dstOp) * byteSizeBit
      -- Segment registers are defined as 32 or 64 bit vectors in order to
      -- avoid having to simulate the GDT. This definition allows users to
      -- directly define their segments offset.
      node = (case (value dstOp) of
        (Reg reg) | isSegmentReg reg -> ExtractExpr 0 wordSizeBit tmpNode
        _ -> tmpNode)
        where tmpNode = case (value srcOp) of
                (Reg reg) | isSegmentReg reg -> ExtractExpr 0 dstSizeBit srcAst
                _ -> srcAst
      undef = case (value srcOp) of
        (Reg reg) | isControlReg reg -> True
        _ -> case (value dstOp) of
          (Reg reg) | isControlReg reg -> True
          _ -> False
  in Compound (fromIntegral (address inst))
      ([incInsnPtr modes inst,
      storeStmt modes dstOp node]
      ++ includeIf undef
          [SetReg undefined (fromX86Flag X86FlagAf) (UndefinedExpr 1),
          SetReg undefined (fromX86Flag X86FlagPf) (UndefinedExpr 1),
          SetReg undefined (fromX86Flag X86FlagSf) (UndefinedExpr 1),
          SetReg undefined (fromX86Flag X86FlagZf) (UndefinedExpr 1),
          SetReg undefined (fromX86Flag X86FlagCf) (UndefinedExpr 1),
          SetReg undefined (fromX86Flag X86FlagOf) (UndefinedExpr 1)])

-- MOVZX instruction

liftX86 modes inst | getInsnId inst == X86InsMovzx =
  let (dstOp : srcOp : _ ) = x86operands inst
      dstAst = getOperandAst modes dstOp
      srcAst = getOperandAst modes srcOp
      dstSizeBit = (fromIntegral $ size dstOp) * byteSizeBit
      zxNode = ZxExpr dstSizeBit srcAst
  in Compound (fromIntegral (address inst))
      [incInsnPtr modes inst,
      storeStmt modes dstOp zxNode]

-- JMP instruction

liftX86 modes inst | getInsnId inst == X86InsJmp =
  let (srcOp : _ ) = x86operands inst
      srcAst = getOperandAst modes srcOp
  in Compound (fromIntegral (address inst)) [SetReg undefined (getInsnPtr modes) srcAst]

-- JE instruction

liftX86 modes inst | getInsnId inst == X86InsJe =
  let (srcOp : _ ) = x86operands inst
      srcAst = getOperandAst modes srcOp
      insnPtr = getInsnPtr modes
  in Compound (fromIntegral (address inst))
      [SetReg undefined insnPtr
        (IteExpr (EqualExpr (GetReg (fromX86Flag X86FlagZf)) (BvExpr (toBv 1 1)))
          srcAst
          (nextInstrPtr modes inst))]

-- JNE instruction

liftX86 modes inst | getInsnId inst == X86InsJne =
  let (srcOp : _ ) = x86operands inst
      srcAst = getOperandAst modes srcOp
      insnPtr = getInsnPtr modes
  in Compound (fromIntegral (address inst))
      [SetReg undefined insnPtr
        (IteExpr (EqualExpr (GetReg (fromX86Flag X86FlagZf)) (BvExpr (toBv 0 1)))
          srcAst
          (nextInstrPtr modes inst))]

-- CALL instruction

liftX86 modes inst | getInsnId inst == X86InsCall =
  let (srcOp : _ ) = x86operands inst
      srcAst = getOperandAst modes srcOp
      sp = getStackReg modes
      archByteSize = getArchByteSize modes
      archBitSize = getArchBitSize modes
  in Compound (fromIntegral (address inst))
      [SetReg undefined sp (BvsubExpr (GetReg sp) (BvExpr (toBv archByteSize archBitSize))),
      Store undefined (GetReg sp) (nextInstrPtr modes inst),
      SetReg undefined (getInsnPtr modes) (ZxExpr archBitSize srcAst)]

-- RET instruction

liftX86 modes inst | getInsnId inst == X86InsRet =
  let operands = x86operands inst
      sp = getStackReg modes
      insnPtr = getInsnPtr modes
      archByteSize = getArchByteSize modes
  in Compound (fromIntegral (address inst))
      ([SetReg undefined insnPtr (GetReg sp),
      SetReg undefined sp (BvaddExpr (GetReg sp) (BvExpr (toBv archByteSize (archByteSize * byteSizeBit))))]
      ++ includeIf (length operands > 0)
          (let srcAst = getOperandAst modes (head operands)
          in [SetReg undefined sp (BvaddExpr (GetReg sp) (ZxExpr (archByteSize * byteSizeBit) srcAst))]))

-- LEA instruction

liftX86 modes inst | getInsnId inst == X86InsLea =
  let (dstOp : srcOp : _ ) = x86operands inst
      dstAst = getOperandAst modes dstOp
      dstSize = fromIntegral (size dstOp * byteSizeBit)
      srcEaSize = getArchBitSize modes
  in Compound (fromIntegral (address inst)) [
      incInsnPtr modes inst,
      case value srcOp of
        Imm value ->
          Comment undefined ("Memory operand expected in " ++ show inst ++ ", but immediate value found instead. Ignoring opcode.")
        Reg reg ->
          Comment undefined ("Memory operand expected in " ++ show inst ++ ", but register value found instead. Ignoring opcode.")
        Mem mem ->
          let srcEa = getLeaAst modes mem
              srcEaFitted =
                if dstSize > srcEaSize then
                  ZxExpr dstSize srcEa
                else if dstSize < srcEaSize then
                  ExtractExpr 0 dstSize srcEa
                else srcEa
          in storeStmt modes dstOp srcEaFitted]

-- Otherwise put in a comment to the effect that the instruction is not supported

liftX86 modes inst =
  Compound (fromIntegral (address inst)) [
    -- Increase the instruction pointer so that exec does not hang here
    incInsnPtr modes inst,
    Comment undefined ("Instruction " ++ mnemonic inst ++ " not supported. Ignoring opcode.")]

