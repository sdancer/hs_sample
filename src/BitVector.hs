module BitVector where

import Data.Vector as Vector
import Data.Bits

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

intToBv :: Int -> BitVector

intToBv a = (Vector.singleton (BitVector.convert a), finiteBitSize (0 :: Int))

wordToBv :: Word -> BitVector

wordToBv a = (Vector.singleton a, finiteBitSize (0 :: Word))

empty :: BitVector

empty = (Vector.empty, 0)

-- Same as div but rounds upwards instead of downwards

cdiv :: Int -> Int -> Int

cdiv a b = -(div (-a) b)

bitVector :: Word -> Int -> BitVector

bitVector a b = (generate (cdiv b digitBitSize) (\x -> if x == 0 then a else 0), b)

zero :: BitVector -> BitVector

zero a = bitVector 0 (bvlength a)

one :: BitVector -> BitVector

one a = bitVector 1 (bvlength a)

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

-- Zero extends the given bit-vector by the given amount

zx :: Int -> BitVector -> BitVector

zx a b | a == 0 = b

zx a (bv,bn) | mod bn digitBitSize == 0 =
  zx (a - re) (Vector.snoc bv 0, bn + re) where re = min digitBitSize a

-- Sets excess bits of bit-vector to zero

zx a (bv,bn) =
  let zc = mod (-bn) digitBitSize
      re = min zc a
      msd = (Vector.last bv) .&. (shift (-1 :: Word) (-zc))
  in zx (a - re) (Vector.snoc (Vector.init bv) msd, bn + re)

-- Compares two bit-vectors by zero-extending both and element-wise checking word equality

equal :: BitVector -> BitVector -> Bool

equal (_,an) (_,bn) | an /= bn = error "Bit-vector arguments to EqualExpr have different bit-lengths."

equal a b =
  let ext = mod (-(bvlength a)) digitBitSize
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

bvnegate a = bvadd (bvnot a) (one a)

bvsub :: BitVector -> BitVector -> BitVector

bvsub a b = bvadd a (bvnegate b)

ite :: BitVector -> BitVector -> BitVector -> BitVector

ite a (bv,bn) (cv,cn) | bn == cn = (if equal a (zero a) then cv else bv, bn)

ite _ _ _ = error "Bit-vector arguments to IteExpr have different bit-lengths."

bvlshr :: BitVector -> BitVector -> BitVector

bvlshr (av,an) (bv,bn) =
  let shamt = BitVector.convert (Vector.head bv)
      shamtMod = mod shamt digitBitSize
      shamtDiv = div shamt digitBitSize
      shft ah al = (shift ah (digitBitSize - shamtMod)) .|. (shift al (-shamtMod))
      na = (Vector.drop shamtDiv av, an - shamtDiv*digitBitSize)
      (nnav, _) = zx (shamtDiv*digitBitSize) na
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

bvconcat a b = bvor (bvshl (zx blength a) (intToBv blength)) (zx (bvlength a) b)
  where blength = bvlength b

bvextract :: Int -> Int -> BitVector -> BitVector

bvextract l h a = bvtruncate (h - l) (bvlshr a (intToBv l))

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

