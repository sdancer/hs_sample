module BitVector where

import Data.Vector as Vector
import Data.Bits
import Numeric

-- Convert instance of integral type to instance of some numerical type

convert :: Integral a => Num b => a -> b

convert a = (fromInteger (toInteger a))

-- The number of bits that each "digit" of a bit-vector will have

digitBitSize :: Int

digitBitSize = finiteBitSize (0 :: Word)

-- First item of tuple is bit-vector value. The least significant digit comes first.
-- Second item of tuple is bit-vector size in bits. The bit-vector must have length equal
-- to ceil(size/digitBitSize).

type BitVector = (Vector Word, Int)

bvlength :: BitVector -> Int

bvlength (av,an) = an

empty :: BitVector

empty = (Vector.empty, 0)

-- Same as div but rounds upwards instead of downwards

cdiv :: Int -> Int -> Int

cdiv a b = -(div (-a) b)

wordToBv :: Word -> Int -> BitVector

wordToBv a b = zx b (Vector.singleton a, finiteBitSize (0 :: Word))

intToBv :: Int -> Int -> BitVector

intToBv a b = sx b (Vector.singleton (BitVector.convert a), finiteBitSize (0 :: Word))

bvzero :: BitVector -> BitVector

bvzero a = wordToBv 0 (bvlength a)

bvone :: BitVector -> BitVector

bvone a = wordToBv 1 (bvlength a)

bvxor :: BitVector -> BitVector -> BitVector

bvxor (av,an) (bv,bn) | an == bn = (Vector.zipWith xor av bv, an)

bvxor _ _ = error "Bit-vector arguments to BvxorExpr have different bit-lengths."

bvand :: BitVector -> BitVector -> BitVector

bvand (av,an) (bv,bn) | an == bn = (Vector.zipWith (.&.) av bv, an)

bvand _ _ = error "Bit-vector arguments to BvandExpr have different bit-lengths."

bvor :: BitVector -> BitVector -> BitVector

bvor (av,an) (bv,bn) | an == bn = (Vector.zipWith (.|.) av bv, an)

bvor _ _ = error "Bit-vector arguments to BvorExpr have different bit-lengths."

bvnot :: BitVector -> BitVector

bvnot (av,an) = (Vector.map complement av, an)

-- Zero extends the given bit-vector to the given amount

zx :: Int -> BitVector -> BitVector

zx a b | a <= bvlength b = bvtruncate a b

zx a (bv,bn) | mod bn digitBitSize == 0 =
  zx a (Vector.snoc bv 0, bn + re) where re = min digitBitSize a

-- Sets excess bits of bit-vector to zero

zx a (bv,bn) =
  let zc = mod (-bn) digitBitSize
      re = min zc (a-bn)
      msd = (Vector.last bv) .&. (shift (-1) (-zc))
  in zx a (Vector.snoc (Vector.init bv) msd, bn + re)

-- Gets the given bit of the bit-vector

bvbit :: BitVector -> Int -> Bool

bvbit bv idx = (bvToWord $ bvextract idx (idx + 1) bv) == 1

-- Sign extends the given bit-vector to the given amount

sx :: Int -> BitVector -> BitVector

sx a b | a <= bvlength b = bvtruncate a b

sx a (bv,bn) | mod bn digitBitSize == 0 =
  sx a (Vector.snoc bv (if bvbit (bv,bn) (bn-1) then -1 else 0), bn + re) where re = min digitBitSize a

-- Sets excess bits of bit-vector to sign

sx a (bv,bn) =
  let zc = mod (-bn) digitBitSize
      oc = mod bn digitBitSize
      re = min zc (a-bn)
      msd = (if bvbit (bv,bn) (bn-1) then (.&.) (shift (-1) (-zc)) else (.|.) (shift (-1) oc)) $ Vector.last bv
  in sx a (Vector.snoc (Vector.init bv) msd, bn + re)

-- Compares two bit-vectors by zero-extending both and element-wise checking word equality

bvequal :: BitVector -> BitVector -> Bool

bvequal (_,an) (_,bn) | an /= bn = error "Bit-vector arguments to EqualExpr have different bit-lengths."

bvequal a b =
  let ext = Vector.length (fst a) * digitBitSize
      (av,_) = zx ext a
      (bv,_) = zx ext b
  in if Vector.and (Vector.zipWith (==) av bv) then True else False

-- Truncates the supplied bit-vector to the given length.

bvtruncate :: Int -> BitVector -> BitVector

bvtruncate a (bv,bn) | a > bn = error "Truncation length is larger than bit-length."

bvtruncate a (bv,bn) = (Vector.take (cdiv a digitBitSize) bv,a)

bvadd :: BitVector -> BitVector -> BitVector

bvadd (av,an) (bv,bn) | an == bn =
  let addc (acc,c) (a,b) =
        let apb = a + b -- a+b modulo 2^digitBitSize
            nc = apb < a -- a+b has carry if it is less than a
            napb = apb + BitVector.convert (fromEnum c) -- add on the carry from the previous addition
            nnc = nc || (napb < apb) -- (a+b)+c has carry if it is less than a+b
        in (Vector.snoc acc napb, nnc)
  in (fst (Vector.foldl addc (Vector.empty, False) (Vector.zip av bv)), an)

bvadd _ _ = error "Bit-vector arguments to BvaddExpr have different bit-lengths."

bvnegate :: BitVector -> BitVector

bvnegate a = bvadd (bvnot a) (bvone a)

bvsub :: BitVector -> BitVector -> BitVector

bvsub a b = bvadd a (bvnegate b)

bvmul :: BitVector -> BitVector -> BitVector

bvmul a b | bvequal b (bvzero b) = bvzero a

bvmul a b = if bvbit b 0 then bvadd a rst else rst
  where rst = bvmul (bvshl a (bvone a)) (bvlshr b (bvone b))

ite :: BitVector -> BitVector -> BitVector -> BitVector

ite a (bv,bn) (cv,cn) | bn == cn = (if bvequal a (bvzero a) then cv else bv, bn)

ite _ _ _ = error "Bit-vector arguments to IteExpr have different bit-lengths."

bvlshr :: BitVector -> BitVector -> BitVector

bvlshr (av,an) (bv,bn) =
  let shamt = BitVector.convert (Vector.head bv)
      shamtMod = mod shamt digitBitSize
      shamtDiv = div shamt digitBitSize
      shft ah al = (shift ah (digitBitSize - shamtMod)) .|. (shift al (-shamtMod))
      na = (Vector.drop shamtDiv av, an - shamtDiv*digitBitSize)
      (nnav, _) = zx an na
  in (Vector.zipWith shft (Vector.snoc (Vector.drop 1 nnav) 0) nnav, an)

bvshl :: BitVector -> BitVector -> BitVector

bvshl (av,an) (bv,bn) =
  let shamt = BitVector.convert (Vector.head bv)
      shamtMod = mod shamt digitBitSize
      shamtDiv = div shamt digitBitSize
      shft ah al = (shift ah shamtMod) .|. (shift al (shamtMod - digitBitSize))
      nav = (Vector.replicate shamtDiv 0) Vector.++ (Vector.take (Vector.length av - shamtDiv) av)
  in (Vector.zipWith shft nav (Vector.cons 0 (Vector.take (Vector.length av - 1) nav)), an)

bvconcat :: BitVector -> BitVector -> BitVector

bvconcat a b = bvor (bvshl (zx newlength a) (intToBv (bvlength b) digitBitSize)) (zx newlength b)
  where newlength = bvlength a + bvlength b

bvextract :: Int -> Int -> BitVector -> BitVector

bvextract l h a = bvtruncate (h - l) (bvlshr a (intToBv l digitBitSize))

bvreplace :: BitVector -> Int -> BitVector -> BitVector

bvreplace a b c = bvconcat (bvextract (b + bvlength c) (bvlength a) a) (bvconcat c (bvtruncate b a))

bvToWord :: BitVector -> Word

bvToWord a | bvlength a == 0 = 0

bvToWord (av,an) | an <= digitBitSize = (Vector.head av) .&. (shift (-1 :: Word) (an-digitBitSize))

bvToWord _ = error "Bit-vector too large to be converted to word."

bvToInt :: BitVector -> Int

bvToInt (av,an) | 0 < an && an <= digitBitSize =
  let rv = Vector.head av
  in BitVector.convert (if testBit rv (an - 1) then
    rv .|. (shift (-1) an)
  else
    rv .&. (shift (-1 :: Word) (an-digitBitSize)))

bvToInt _ = error "Bit-vector cannot be converted to int."

bvshow :: BitVector -> [Char]

bvshow a | bvlength a == 0 = ""

bvshow a | bvlength a <= 4 = showHex (bvToWord (zx 4 a)) ""

bvshow a = (bvshow (bvextract 4 (bvlength a) a)) Prelude.++ (bvshow (bvtruncate 4 a))

