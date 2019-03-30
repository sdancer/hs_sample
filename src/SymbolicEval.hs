module SymbolicEval where

import EvalAst
import Ast
import Hapstone.Internal.X86 as X86
import Util
import Data.Bits
import Hapstone.Internal.Capstone as Capstone

-- Simplifies the given expression in the given context

symEval :: ExecutionContext -> Expr -> Expr

symEval cin (BvxorExpr c d) =
  case (symEval cin c, symEval cin d) of
    (BvExpr a an, BvExpr b bn) | an == bn -> BvExpr (xor a b) an
    (BvExpr _ _, BvExpr _ _) -> error "Bit-vector arguments to BvxorExpr have different bit-lengths."
    (c, d) -> BvxorExpr c d

symEval cin (BvandExpr c d) =
  case (symEval cin c, symEval cin d) of
    (BvExpr a an, BvExpr b bn) | an == bn -> BvExpr (a .&. b) an
    (BvExpr _ _, BvExpr _ _) -> error "Bit-vector arguments to BvandExpr have different bit-lengths."
    (c, d) -> BvandExpr c d

symEval cin (BvorExpr c d) =
  case (symEval cin c, symEval cin d) of
    (BvExpr a an, BvExpr b bn) | an == bn -> BvExpr (a .|. b) an
    (BvExpr _ _, BvExpr _ _) -> error "Bit-vector arguments to BvorExpr have different bit-lengths."
    (c, d) -> BvorExpr c d

symEval cin (BvnotExpr c) =
  case (symEval cin c) of
    (BvExpr a an) -> BvExpr (complement a) an
    (c) -> BvnotExpr c

symEval cin (EqualExpr c d) =
  case (symEval cin c, symEval cin d) of
    (BvExpr a an, BvExpr b bn) | an == bn -> BvExpr (if a == b then 1 else 0) an
    (BvExpr _ _, BvExpr _ _) -> error "Bit-vector arguments to EqualExpr have different bit-lengths."
    (c, d) -> EqualExpr c d

symEval cin (BvaddExpr c d) =
  case (symEval cin c, symEval cin d) of
    (BvExpr a an, BvExpr b bn) | an == bn -> BvExpr (a + b) an
    (BvExpr _ _, BvExpr _ _) -> error "Bit-vector arguments to BvaddExpr have different bit-lengths."
    (c, d) -> BvaddExpr c d

symEval cin (BvsubExpr c d) =
  case (symEval cin c, symEval cin d) of
    (BvExpr a an, BvExpr b bn) | an == bn -> BvExpr (a - b) an
    (BvExpr _ _, BvExpr _ _) -> error "Bit-vector arguments to BvsubExpr have different bit-lengths."
    (c, d) -> BvsubExpr c d

symEval cin (BvlshrExpr c d) =
  case (symEval cin c, symEval cin d) of
    (BvExpr a an, BvExpr b _) -> BvExpr (convert (shift ((convert a) :: Word) (-b))) an
    (c, d) -> BvlshrExpr c d

symEval cin (ZxExpr a c) =
  case (symEval cin c) of
    (BvExpr b bn) -> BvExpr b (bn + a)
    (c) -> ZxExpr a c

symEval cin (IteExpr a b c) =
  case (symEval cin a, symEval cin b, symEval cin c) of
    (BvExpr a _, b, c) -> if a /= 0 then b else c
    (a, b, c) -> IteExpr a b c

symEval cin (ReplaceExpr a b c d) =
  case (symEval cin c, symEval cin d) of
    (BvExpr c cn, BvExpr d dn) | dn == b+1-a ->
      BvExpr ((c .&. (complement (oneBitsBetween a b))) .|. shift d b) cn
    (BvExpr _ _, BvExpr _ _) -> error "Size of replacement bit-vector does not match target space."
    (c, d) -> ReplaceExpr a b c d

symEval cin (ExtractExpr a b c) =
  case (symEval cin c) of
    (BvExpr c cn) -> BvExpr ((shift c (-b)) .&. (oneBitsUpto (a + 1 - b))) (a + 1 - b)
    (c) -> ExtractExpr a b c

symEval cin (GetReg bs) =
  let regVal = getRegisterValue (reg_file cin) bs
  in case regVal of
    Just x -> BvExpr x (getRegSize bs)
    Nothing -> GetReg bs

-- add ro memory (raise exception if written)
-- add symbolic addressed memory (example stack, no concrete values mapping references)

symEval cin (Load a b) =
  case (symEval cin b) of
    (BvExpr memStart bn) ->
      let memVal = getMemoryValue (memory cin) [memStart..(memStart + a - 1)]
      in case memVal of
        Just x -> BvExpr x a
        Nothing -> Load a (BvExpr memStart bn)
    (b) -> Load a b

symEval cin expr = expr

symExec :: ExecutionContext -> Stmt -> (ExecutionContext, Stmt)

-- Symbolically executes a SetReg operation by either simplifying assignment value to a
-- literal then putting it into the register file, or by undefining the target register
-- to keep later statements symbolic.

symExec cin (SetReg bs a) =
  let c = symEval cin a
      nreg = case c of
        BvExpr a an -> update_reg_file (reg_file cin) bs a
        _ -> let treg_file = reg_file cin
                 old_ranges = ranges treg_file
                 tvalues = values treg_file
              in RegisterFile {ranges = removeRegister old_ranges bs, values = tvalues}
  in (cin { reg_file = nreg }, SetReg bs c)


symExec cin (Store n dst val) =
  let pdest = symEval cin dst
      pval = symEval cin val
      bytes bs = shift bs (0-3)
  in
      case pdest of
        BvExpr a bitsize -> (updateMemory cin (bytes bitsize) a 0x1337, Store n pdest pval)
        _ -> error $ "Store on symbolic mem not implemented"

updateMemory :: ExecutionContext -> Int -> Int -> Int -> ExecutionContext
updateMemory cin bc address val =
    let nmem = foldl (\mem x -> assign mem ((address+x), (shift val (0-(8 * x))) .&. 0xff)) (memory cin) [0..bc-1]
    in cin { memory = nmem }

-- Executes a group of statements pointed to by the instruction pointer and returns the
-- new context

symSteps :: ExecutionContext -> (ExecutionContext, [(Int, [Stmt])])

symSteps cin | stmts cin == [] = (cin, [])

symSteps cin =
    let x = snd (head (stmts cin)) in
      let process ec [] ns = (ec {
            stmts = tail (stmts ec)
          }, (fst (head (stmts cin)), reverse ns))
          process ec (x:xs) ns =
            let (nec, s) = symExec ec x
            in process nec xs (s:ns)
          (fec, ent) = process cin x []
          (ffec, ents) = symSteps fec
      in (ffec, (ent:ents))
