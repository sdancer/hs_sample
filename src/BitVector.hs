module BitVector where

import Data.Vector as Vector
import Data.Bits
import Numeric

-- Convert instance of integral type to instance of some numerical type

convert :: Integral a => Num b => a -> b

convert a = (fromInteger (toInteger a))

-- First item of tuple is bit-vector value. The least significant digit comes first.
-- Second item of tuple is bit-vector size in bits. The bit-vector must have length equal
-- to ceil(size/digitBitSize).

type BitVector = (Integer, Int)

bvlength :: BitVector -> Int

bvlength (av,an) = an

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

zx a (bv,bn) = ((bit bn - 1) .&. bv, a)

-- Gets the given bit of the bit-vector

bvbit :: BitVector -> Int -> Bool

bvbit (bv,bn) idx = testBit bv idx

-- Sign extends the given bit-vector to the given amount

sx :: Int -> BitVector -> BitVector

sx a (bv,bn) = ((-(bit bn)) .|. bv, a)

-- Compares two bit-vectors by zero-extending both and element-wise checking word equality

bvequal :: BitVector -> BitVector -> Bool

bvequal (_,an) (_,bn) | an /= bn = error "Bit-vector arguments to EqualExpr have different bit-lengths."

bvequal (av,an) (bv,bn) = (av .&. (bit an - 1)) == (bv .&. (bit bn - 1))

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

--bvult :: BitVector -> BitVector -> BitVector

--bvult ([],an) ([],bn) =

--bvult (av,an) (bv,bn) | an > 0 && last av \= last bv = last av < last bv

--bvult (av,an) (bv,bn) | an > 0 && last av == last bv = last av < last bv

ite :: BitVector -> BitVector -> BitVector -> BitVector

ite (av,an) (bv,bn) (cv,cn) | bn == cn = (if av == 0 then cv else bv, bn)

ite _ _ _ = error "Bit-vector arguments to IteExpr have different bit-lengths."

bvlshr :: BitVector -> BitVector -> BitVector

bvlshr (av,an) (bv,bn) = (shiftR (av .&. (bit an - 1)) (fromInteger bv), an)

bvshl :: BitVector -> BitVector -> BitVector

bvshl (av,an) (bv,bn) = (shiftL av (fromInteger bv), an)

bvconcat :: BitVector -> BitVector -> BitVector

bvconcat a b = bvor (bvshl (zx newlength a) (toInteger $ bvlength b, newlength)) (zx newlength b)
  where newlength = bvlength a + bvlength b

bvextract :: Int -> Int -> BitVector -> BitVector

bvextract l h a = zx (h - l) (bvlshr a (toInteger l, bvlength a))

bvreplace :: BitVector -> Int -> BitVector -> BitVector

bvreplace a b c = bvconcat (bvextract (b + bvlength c) (bvlength a) a) (bvconcat c (zx b a))

fromBv :: Num a => BitVector -> a

fromBv (av,an) = fromInteger ((-(bit an)) .|. av)

toBv :: Integral a => a -> Int -> BitVector

toBv a b = (toInteger a, b)

