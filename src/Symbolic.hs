module Symbolic where

import           Asm
import           Data.Bits
import qualified Data.ByteString            as BS
import qualified Data.List                  as List
import           Data.Map.Strict
import           Data.Word
import           Debug.Trace
import           Hapstone.Capstone
import           Hapstone.Internal.Capstone as Capstone
import           Hapstone.Internal.X86      as X86
import           Numeric                    (showHex)
import           Util

-- Virtual proccess instruction
vproc :: ProccessedInsn -> State -> (ProccessedInsn, State)
vproc (Insn insn) state = trace ("vproc " ++ insn_to_str insn) $ if contains_group X86GrpCall insn
  then case get_first_opr_value insn of
    Imm addr  -> (Skip insn, push_to_stack (NumVal addr) state)
    otherwise -> error "call with nothing???"
  else case mnemonic insn of
    "push" -> do
      let op0 = get_first_opr_value insn
      let value = fetch_contents op0 state
      (Skip insn, push_to_stack value state)
    "pushfd" -> do
      let value = eflags $ regs state
      (Skip insn, push_to_stack (Flags value) state)
    "pop" -> (Skip insn, do_pop (get_first_opr_value insn) state)
    "pushal" -> do
      let s1 = push_to_stack (fetch_reg_contents X86RegEax state) state
      let s2 = push_to_stack (fetch_reg_contents X86RegEbx s1) s1
      let s3 = push_to_stack (fetch_reg_contents X86RegEcx s2) s2
      let s4 = push_to_stack (fetch_reg_contents X86RegEdx s3) s3
      let s5 = push_to_stack (NumVal 0) s4
      let s6 = push_to_stack (fetch_reg_contents X86RegEbp s5) s5
      let s7 = push_to_stack (fetch_reg_contents X86RegEsi s6) s6
      let s8 = push_to_stack (fetch_reg_contents X86RegEdi s7) s7
      (Skip insn, s8)
    "popal" -> do
      let s1 = do_pop (Reg X86RegEdi) state
      let s2 = do_pop (Reg X86RegEsi) s1
      let s3 = do_pop (Reg X86RegEbp) s2
      let s4 = do_pop (Reg X86RegInvalid) s3
      let s5 = do_pop (Reg X86RegEdx) s4
      let s6 = do_pop (Reg X86RegEcx) s5
      let s7 = do_pop (Reg X86RegEbx) s6
      let s8 = do_pop (Reg X86RegEax) s7
      (Skip insn, s8)
    "mov" -> do
      let src = get_second_opr_value insn
      let value = fetch_contents src state
      let dst = get_first_opr_value insn
      case put_contents dst value state of
         (new_state, True)  -> (WallSE insn, new_state)
         (new_state, False) -> (Skip insn, new_state)
    "xchg" -> do
      let op0 = get_first_opr_value insn
      let op1 = get_second_opr_value insn
      let value0 = fetch_contents op0 state
      let value1 = fetch_contents op1 state
      let (new_state, _) = put_contents op0 value1 state
      let (new_state2, _) = put_contents op0 value1 new_state
      (Skip insn, new_state2)
    "inc" -> (Skip insn, do_aritm (aritm_add, abstract_add) (get_first_opr_value insn) (Imm 1) state)
    "dec" -> (Skip insn, do_aritm (aritm_sub, abstract_sub) (get_first_opr_value insn) (Imm 1) state)
    "add" -> (Skip insn, do_aritm (aritm_add, abstract_add) (get_first_opr_value insn) (get_second_opr_value insn) state)
    "sub" -> (Skip insn, do_aritm (aritm_sub, abstract_sub) (get_first_opr_value insn) (get_second_opr_value insn) state)
    "or" ->  (Skip insn, do_aritm (aritm_or, abstract_or) (get_first_opr_value insn) (get_second_opr_value insn) state)
    "xor" -> (Skip insn, do_aritm (aritm_xor, abstract_xor) (get_first_opr_value insn) (get_second_opr_value insn) state)
    "shl" -> (Skip insn, do_aritm (aritm_shl, abstract_shl) (get_first_opr_value insn) (get_second_opr_value insn) state)
    "shr" -> (Skip insn, do_aritm (aritm_shr, abstract_shr) (get_first_opr_value insn) (get_second_opr_value insn) state)
    "and" -> (Skip insn, do_aritm (aritm_and, abstract_and) (get_first_opr_value insn) (get_second_opr_value insn) state)
    "neg" -> (Skip insn, do_aritm (aritm_neg, abstract_neg) (get_first_opr_value insn) (Imm 0) state) -- second oprand should be nothing
    "not" -> (Skip insn, do_aritm (aritm_not, abstract_not) (get_first_opr_value insn) (Imm 0) state) -- second oprand should be nothing
    otherwise -> trace ("## WARN: unimplement insn at " ++ showHex (address insn) "") $ (Skip insn, state)
vproc x state = (x, state) -- do nothing with skip and break instruction

push_to_stack :: AsmValue -> State -> State
push_to_stack value state = case fetch_reg_contents X86RegEsp state of
  NumVal esp_val -> do
    let stoff = esp_val - 4
    let s = set_reg_contents X86RegEsp (NumVal stoff) state
    let nstack = trace ("# push " ++ show value ++ " to " ++ show stoff ++ " in " ++ show(keys $ stack $ s)) $ insert stoff value $ stack s
    let new_state2 = s {stack = nstack}
    trace ("# after push " ++ show(keys $ stack $ new_state2)) $ new_state2
  otherwise -> error "bad esp"

pop_from_stack :: Word64 -> State -> (AsmValue, State)
pop_from_stack pos state = do
    let value = trace ("# pop from " ++ show pos ++ " in " ++ show(keys $ stack $ state)) $ (stack state) ! pos
    let new_stack_pos = NumVal (pos + 4)
    let new_state = set_reg_contents X86RegEsp new_stack_pos state
    -- let nstack = delete pos $ stack new_state
    (value, new_state)

do_pop :: CsX86OpValue -> State -> State
do_pop (Reg X86RegInvalid) state = case fetch_reg_contents X86RegEsp state of
    NumVal cur_stack_pos -> do
      let (value, new_state) = trace ("# pop to invalid reg, popal ??") $ pop_from_stack cur_stack_pos state
      new_state
    otherwise -> error "bad esp"
do_pop dest state = case fetch_reg_contents X86RegEsp state of
    NumVal cur_stack_pos -> do
      let (value, new_state) = pop_from_stack cur_stack_pos state
      let (new_state2, _) = put_contents dest value new_state
      trace ("# --> pop to " ++ show dest ++ " ..... new ESP=" ++ show(fetch_reg_contents X86RegEsp new_state2) ++ " value = " ++ show value) $ new_state2
    otherwise -> error "bad esp"

type RealOpHandler = (Word64 -> Word64 -> Word64)
type AbstractOpHandler = (AsmValue -> AsmValue -> AsmValue)

do_aritm :: (RealOpHandler, AbstractOpHandler) -> CsX86OpValue -> CsX86OpValue -> State -> State
do_aritm (real_op_hdl, abs_op_hdl) op1 op2 state = do
  -- let op1 = get_first_opr_value insn
  -- let op2 = get_second_opr_value insn
  let op1v = fetch_contents op1 state
  let op2v = fetch_contents op2 state
  case (op1v, op2v) of
    (NumVal v1, NumVal v2) -> do
      let val = real_op_hdl v1 v2
      let val2 = val .&. 0xffffffff
      let (new_state, _) = put_contents op1 (NumVal val2) state
      new_state
    otherwise -> do
      let val = abs_op_hdl op1v op2v
      let (new_state, _) = put_contents op1 val state
      new_state

aritm_add :: Word64 -> Word64 -> Word64
aritm_add a b = fromIntegral( a32 + b32 )::Word64
  where
    a32 = fromIntegral(a)::Word32
    b32 = fromIntegral(b)::Word32

aritm_sub :: Word64 -> Word64 -> Word64
aritm_sub a b = fromIntegral( a32 + b32 )::Word64
  where
    a32 = fromIntegral(a)::Word32
    b32 = fromIntegral(b)::Word32

aritm_or :: Word64 -> Word64 -> Word64
aritm_or a b = a .|. b

aritm_xor :: Word64 -> Word64 -> Word64
aritm_xor a b = xor a b

aritm_shl :: Word64 -> Word64 -> Word64
aritm_shl a b = fromIntegral(shift a32 b32)::Word64
  where
    a32 = fromIntegral(a)::Int
    b32 = fromIntegral(b)::Int

aritm_shr :: Word64 -> Word64 -> Word64
aritm_shr a b = fromIntegral(shift a32 (-b32))::Word64
  where
    a32 = fromIntegral(a)::Int
    b32 = fromIntegral(b)::Int

aritm_and :: Word64 -> Word64 -> Word64
aritm_and a b = a .&. b

aritm_neg :: Word64 -> Word64 -> Word64
aritm_neg a _ = 0 - a

aritm_not :: Word64 -> Word64 -> Word64
aritm_not a _ = fromIntegral(complement a32)::Word64
  where
    a32 = fromIntegral(a)::Word32

real_img :: AbstractOp -> Maybe (Word64, AsmValue)
real_img a = case op1 a of
  NumVal x -> Just (x, op2 a)
  otherwise -> case op2 a of
    NumVal x  -> Just (x, op1 a)
    otherwise -> Nothing

-- try to make first value is an integer
real_img_pair :: AsmValue -> AsmValue -> Maybe (Word64, AsmValue)
real_img_pair (NumVal a) b = Just (a, b)
real_img_pair a (NumVal b) = Just (b, a)
real_img_pair _ _          = Nothing

-- check abstract expression can be simplity
val_in_abstract_ops :: AsmValue -> [Char] -> AsmValue -> Bool
val_in_abstract_ops (AbsVal a) ot b = ((optype a) == ot) && one_op_equal_b
  where
    one_op_equal_b = ((op1 a) == b) || ((op2 a) == b)
val_in_abstract_ops _ _ _ = False

-- simplity expression
exclude_val_from_abstract :: AsmValue -> AsmValue -> AsmValue
exclude_val_from_abstract (AbsVal a) b = if o1 == b
  then o2
  else if o2 == b
    then o1
    else error "wrong input type"
  where
    o1 = op1 a
    o2 = op2 a

select_real :: AsmValue -> AsmValue -> Maybe Word64
select_real (NumVal a) _ = Just a
select_real _ (NumVal b) = Just b
select_real _ _          = Nothing

select_abs :: AsmValue -> AsmValue -> Maybe AbstractOp
select_abs (AbsVal a) _ = Just a
select_abs _ (AbsVal b) = Just b
select_abs _ _          = Nothing

real_part :: AbstractOp -> Maybe Word64
real_part op = case op1 op of
  NumVal x -> Just x
  otherwise -> case op2 op of
    NumVal x  -> Just x
    otherwise -> Nothing

abs_part :: AbstractOp -> Maybe AbstractOp
abs_part op = case op1 op of
  AbsVal x -> Just x
  otherwise -> case op2 op of
    AbsVal x  -> Just x
    otherwise -> Nothing

abstract_add :: AsmValue -> AsmValue -> AsmValue
abstract_add a b = case real_img_pair a b of
  Just (av, bv) -> case bv of
    AbsVal abs_val -> case real_img abs_val of
      Just (x, y) -> case optype abs_val of
        "add" -> AbsVal $ AbstractOp {optype="add", op1=y, op2=NumVal(av + x)}
        "sub" -> AbsVal $ AbstractOp {optype="add", op1=y, op2=NumVal(av - x)}
        otherwise -> AbsVal $ AbstractOp {optype="add", op1=a, op2=b}
      otherwise -> AbsVal $ AbstractOp {optype="add", op1=a, op2=b}
    otherwise -> AbsVal $ AbstractOp {optype="add", op1=a, op2=b}
  otherwise -> AbsVal $ AbstractOp {optype="add", op1=a, op2=b}


abstract_sub :: AsmValue -> AsmValue -> AsmValue
abstract_sub a b = case real_img_pair a b of
  Just (av, bv) -> case bv of
    AbsVal abs_val -> case real_img abs_val of
      Just (x, y) -> case optype abs_val of
        "add" -> AbsVal $ AbstractOp {optype="sub", op1=y, op2=NumVal(av + x)}
        "sub" -> AbsVal $ AbstractOp {optype="sub", op1=y, op2=NumVal(av - x)}
        otherwise -> AbsVal $ AbstractOp {optype="sub", op1=a, op2=b}
      otherwise -> AbsVal $ AbstractOp {optype="sub", op1=a, op2=b}
    otherwise -> AbsVal $ AbstractOp {optype="sub", op1=a, op2=b}
  otherwise -> AbsVal $ AbstractOp {optype="sub", op1=a, op2=b}

abstract_or :: AsmValue -> AsmValue -> AsmValue
abstract_or a b = AbsVal $ AbstractOp {optype="or", op1=a, op2=b}

abstract_xor :: AsmValue -> AsmValue -> AsmValue
abstract_xor a b
  | a == b = NumVal 0
  | otherwise = if val_in_abstract_ops b "xor" a
    then exclude_val_from_abstract b a -- simplify a xor (b xor c) = a xor c
    else if val_in_abstract_ops a "xor" b
      then exclude_val_from_abstract a b -- simplify a xor (b xor c) = a xor c
      else case (select_real a b, select_abs a b) of
        (Just real, Just abstract) -> if (optype abstract) == "xor"
          then case (real_part abstract, abs_part abstract) of
            (Just realp, Just absp) -> do
              let val = 0xffffffff .&. (xor real realp)
              if val == 0
                then AbsVal absp
                else AbsVal $ AbstractOp {optype="xor", op1=AbsVal absp, op2=NumVal val}
            otherwise -> AbsVal $ AbstractOp {optype="xor", op1=a, op2=b}
          else AbsVal $ AbstractOp {optype="xor", op1=a, op2=b}
        otherwise -> AbsVal $ AbstractOp {optype="xor", op1=a, op2=b}

abstract_shl :: AsmValue -> AsmValue -> AsmValue
abstract_shl a b = AbsVal $ AbstractOp {optype="shl", op1=a, op2=b}

abstract_shr :: AsmValue -> AsmValue -> AsmValue
abstract_shr a b = AbsVal $ AbstractOp {optype="shr", op1=a, op2=b}

abstract_and :: AsmValue -> AsmValue -> AsmValue
abstract_and a b = AbsVal $ AbstractOp {optype="and", op1=a, op2=b}

abstract_neg :: AsmValue -> AsmValue -> AsmValue
abstract_neg a b = AbsVal $ AbstractOp {optype="neg", op1=a, op2=b}

abstract_not :: AsmValue -> AsmValue -> AsmValue
abstract_not a b = AbsVal $ AbstractOp {optype="not", op1=a, op2=b}
