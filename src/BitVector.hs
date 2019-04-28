module BitVector where

import Data.Bits
import Numeric
import Numeric.Natural

-- First item of tuple is bit-vector value. The least significant digit comes first.
-- Second item of tuple is bit-vector size in bits. The bit-vector must have length equal
-- to ceil(size/digitBitSize).

newtype BitVector = Bv (Integer, Int) deriving Show

bvlength :: BitVector -> Int

bvlength (Bv (av, an)) = an

bvtrue :: BitVector

bvtrue = Bv (1, 1)

bvfalse :: BitVector

bvfalse = Bv (0, 1)

boolToBv :: Bool -> BitVector

boolToBv True = bvtrue

boolToBv False = bvfalse

empty :: BitVector

empty = Bv (0, 0)

bvzero :: Int -> BitVector

bvzero a = Bv (0, a)

bvone :: Int -> BitVector

bvone a = Bv (1, a)

bvxor :: BitVector -> BitVector -> BitVector

bvxor (Bv (av,an)) (Bv (bv,bn)) | an == bn = Bv (xor av bv, an)

bvxor _ _ = error "Bit-vector arguments to BvxorExpr have different bit-lengths."

bvand :: BitVector -> BitVector -> BitVector

bvand (Bv (av,an)) (Bv (bv,bn)) | an == bn = Bv (av .&. bv, an)

bvand _ _ = error "Bit-vector arguments to BvandExpr have different bit-lengths."

bvor :: BitVector -> BitVector -> BitVector

bvor (Bv (av,an)) (Bv (bv,bn)) | an == bn = Bv (av .|. bv, an)

bvor _ _ = error "Bit-vector arguments to BvorExpr have different bit-lengths."

bvnot :: BitVector -> BitVector

bvnot (Bv (av,an)) = Bv (complement av, an)

-- Zero extends the given bit-vector to the given amount

zx :: Int -> BitVector -> BitVector

zx a b = Bv (fromBvU b, a)

-- Gets the given bit of the bit-vector

bvbit :: BitVector -> Int -> Bool

bvbit (Bv (bv, bn)) idx = testBit bv idx

-- Sign extends the given bit-vector to the given amount

sx :: Int -> BitVector -> BitVector

sx a b = Bv (fromBvS b, a)

bvudiv :: BitVector -> BitVector -> BitVector

bvudiv a b | bvlength a == bvlength b = Bv (div (fromBvU a) (fromBvU b), bvlength a)

bvudiv _ _ = error "Bit-vector arguments to BvudivExpr have different bit-lengths."

bvsdiv :: BitVector -> BitVector -> BitVector

bvsdiv a b | bvlength a == bvlength b = Bv (div (fromBvS a) (fromBvS b), bvlength a)

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

bvlshr a b = Bv (shiftR (fromBvU a) (fromBvU b), bvlength a)

bvashr :: BitVector -> BitVector -> BitVector

bvashr a b = Bv (shiftR (fromBvS a) (fromBvU b), bvlength a)

bvshl :: BitVector -> BitVector -> BitVector

bvshl a b = Bv (shiftL (fromBvU a) (fromBvU b), bvlength a)

bvconcat :: BitVector -> BitVector -> BitVector

bvconcat a b = bvor (bvshl (zx newlength a) (Bv (toInteger $ bvlength b, newlength))) (zx newlength b)
  where newlength = bvlength a + bvlength b

bvextract :: Int -> Int -> BitVector -> BitVector

bvextract l h a = zx (h - l) (bvlshr a (Bv (toInteger l, bvlength a)))

bvreplace :: BitVector -> Int -> BitVector -> BitVector

bvreplace a b c = bvconcat (bvextract (b + bvlength c) (bvlength a) a) (bvconcat c (zx b a))

fromBvU :: Num a => BitVector -> a

fromBvU (Bv (av,an)) = fromInteger ((bit an - 1) .&. av)

fromBvS :: Num a => BitVector -> a

fromBvS (Bv (av,an)) = fromInteger (if testBit av (an-1) then (-(bit an)) .|. av else (bit an - 1) .&. av)

toBv :: Integral a => a -> Int -> BitVector

toBv a b = Bv (toInteger a, b)

-- The following instances are required to enable pattern matching on bit-vectors

instance Num BitVector where
  Bv (av,an) + Bv (bv,bn) | an == bn = Bv (av+bv, an)
  _ + _ = error "Bit-vector arguments to BvaddExpr have different bit-lengths."
  Bv (av,an) - Bv (bv,bn) | an == bn = Bv (av-bv, an)
  _ - _ = error "Bit-vector arguments to BvsubExpr have different bit-lengths."
  Bv (av,an) * Bv (bv,bn) | an == bn = Bv (av*bv,an)
  _ * _ = error "Bit-vector arguments to BvmulExpr have different bit-lengths."
  negate (Bv (av, an)) = Bv (-av, an)
  abs (Bv (av, an)) = Bv (abs av, an)
  signum (Bv (av, an)) = Bv (signum av, an)
  -- A hack to enable pattern matching on BitVectors: -1 stands for variable length
  fromInteger x = Bv (x, -1)

instance Eq BitVector where
  -- Comparing a fixed length bit-vector to a variable length bit-vector
  a == Bv (b,-1) = Bv (b,-1) == a
  Bv (a,-1) == b = Bv (a, bvlength b) == b
  -- Comparing two bit-vectors that are fixed to the same length
  a == b | bvlength a == bvlength b = fromBvU a == fromBvU b
  -- Otherwise the two bit-vectors cannot be compared
  _ == _ = error "Bit-vector arguments to EqualExpr have different bit-lengths."

