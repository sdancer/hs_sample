module BitVector where

import Data.Bits
import Numeric
import Numeric.Natural

-- First item of tuple is bit-vector value. The least significant digit comes first.
-- Second item of tuple is bit-vector size in bits. The bit-vector must have length equal
-- to ceil(size/digitBitSize).

type BitVector = (Integer, Int)

bvlength :: BitVector -> Int

bvlength (av,an) = an

bvtrue :: BitVector

bvtrue = (1,1)

bvfalse :: BitVector

bvfalse = (0,1)

boolToBv :: Bool -> BitVector

boolToBv True = bvtrue

boolToBv False = bvfalse

empty :: BitVector

empty = (0, 0)

bvzero :: Int -> BitVector

bvzero a = (0,a)

bvone :: Int -> BitVector

bvone a = (1,a)

bvxor :: BitVector -> BitVector -> BitVector

bvxor (av,an) (bv,bn) | an == bn = (xor av bv, an)

bvxor _ _ = error "Bit-vector arguments to BvxorExpr have different bit-lengths."

bvand :: BitVector -> BitVector -> BitVector

bvand (av,an) (bv,bn) | an == bn = (av .&. bv, an)

bvand _ _ = error "Bit-vector arguments to BvandExpr have different bit-lengths."

bvor :: BitVector -> BitVector -> BitVector

bvor (av,an) (bv,bn) | an == bn = (av .|. bv, an)

bvor _ _ = error "Bit-vector arguments to BvorExpr have different bit-lengths."

bvnot :: BitVector -> BitVector

bvnot (av,an) = (complement av, an)

-- Zero extends the given bit-vector to the given amount

zx :: Int -> BitVector -> BitVector

zx a b = (fromBvU b, a)

-- Gets the given bit of the bit-vector

bvbit :: BitVector -> Int -> Bool

bvbit (bv,bn) idx = testBit bv idx

-- Sign extends the given bit-vector to the given amount

sx :: Int -> BitVector -> BitVector

sx a b = (fromBvS b, a)

-- Compares two bit-vectors by zero-extending both and element-wise checking word equality

bvequal :: BitVector -> BitVector -> Bool

bvequal a b | bvlength a == bvlength b = fromBvU a == fromBvU b

bvequal _ _ = error "Bit-vector arguments to EqualExpr have different bit-lengths."

bvadd :: BitVector -> BitVector -> BitVector

bvadd (av,an) (bv,bn) | an == bn = (av+bv, an)

bvadd _ _ = error "Bit-vector arguments to BvaddExpr have different bit-lengths."

bvnegate :: BitVector -> BitVector

bvnegate (av,an) = (-av, an)

bvsub :: BitVector -> BitVector -> BitVector

bvsub (av,an) (bv,bn) | an == bn = (av-bv, an)

bvsub _ _ = error "Bit-vector arguments to BvsubExpr have different bit-lengths."

bvmul :: BitVector -> BitVector -> BitVector

bvmul (av,an) (bv,bn) | an == bn = (av*bv,an)

bvmul _ _ = error "Bit-vector arguments to BvmulExpr have different bit-lengths."

bvudiv :: BitVector -> BitVector -> BitVector

bvudiv a b | bvlength a == bvlength b = (div (fromBvU a) (fromBvU b), bvlength a)

bvudiv _ _ = error "Bit-vector arguments to BvudivExpr have different bit-lengths."

bvsdiv :: BitVector -> BitVector -> BitVector

bvsdiv a b | bvlength a == bvlength b = (div (fromBvS a) (fromBvS b), bvlength a)

bvsdiv _ _ = error "Bit-vector arguments to BvsdivExpr have different bit-lengths."

bvult :: BitVector -> BitVector -> Bool

bvult a b | bvlength a == bvlength b = fromBvU a < fromBvU b

bvult _ _ = error "Bit-vector arguments to BvultExpr have different bit-lengths."

bvule :: BitVector -> BitVector -> Bool

bvule a b | bvlength a == bvlength b = fromBvU a <= fromBvU b

bvule _ _ = error "Bit-vector arguments to BvuleExpr have different bit-lengths."

bvslt :: BitVector -> BitVector -> Bool

bvslt a b | bvlength a == bvlength b = fromBvS a < fromBvS b

bvslt _ _ = error "Bit-vector arguments to BvsltExpr have different bit-lengths."

bvsle :: BitVector -> BitVector -> Bool

bvsle a b | bvlength a == bvlength b = fromBvS a <= fromBvS b

bvsle _ _ = error "Bit-vector arguments to BvsleExpr have different bit-lengths."

bvugt :: BitVector -> BitVector -> Bool

bvugt a b = bvult b a

bvuge :: BitVector -> BitVector -> Bool

bvuge a b = bvule b a

bvsgt :: BitVector -> BitVector -> Bool

bvsgt a b = bvslt b a

bvsge :: BitVector -> BitVector -> Bool

bvsge a b = bvsle b a

ite :: BitVector -> BitVector -> BitVector -> BitVector

ite a _ _ | bvlength a /= 1 = error "Boolean argument to IteExpr has length unequal to 1."

ite _ b c | bvlength b /= bvlength c = error "Bit-vector arguments to IteExpr have different bit-lengths."

ite a b c = if fromBvU a == 1 then b else c

bvlshr :: BitVector -> BitVector -> BitVector

bvlshr a b = (shiftR (fromBvU a) (fromBvU b), bvlength a)

bvashr :: BitVector -> BitVector -> BitVector

bvashr a b = (shiftR (fromBvS a) (fromBvU b), bvlength a)

bvshl :: BitVector -> BitVector -> BitVector

bvshl (av,an) (bv,bn) = (shiftL av (fromInteger bv), an)

bvconcat :: BitVector -> BitVector -> BitVector

bvconcat a b = bvor (bvshl (zx newlength a) (toInteger $ bvlength b, newlength)) (zx newlength b)
  where newlength = bvlength a + bvlength b

bvextract :: Int -> Int -> BitVector -> BitVector

bvextract l h a = zx (h - l) (bvlshr a (toInteger l, bvlength a))

bvreplace :: BitVector -> Int -> BitVector -> BitVector

bvreplace a b c = bvconcat (bvextract (b + bvlength c) (bvlength a) a) (bvconcat c (zx b a))

fromBvU :: Num a => BitVector -> a

fromBvU (av,an) = fromInteger ((-(bit an)) .|. av)

fromBvS :: Num a => BitVector -> a

fromBvS (av,an) = fromInteger (if testBit av (an-1) then (-(bit an)) .|. av else (bit an - 1) .&. av)

toBv :: Integral a => a -> Int -> BitVector

toBv a b = (toInteger a, b)

