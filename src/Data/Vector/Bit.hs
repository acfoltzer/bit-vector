module Data.Vector.Bit where

import Data.Bits
import qualified Data.Vector as V

type BitVector = V.Vector Bool

unpack :: (Bits a) => a -> BitVector
unpack w = V.generate (bitSize w) (testBit w)

pack :: (Bits a) => BitVector -> a
pack v = V.ifoldl' set zero v
  where
    set w i True = w `setBit` i
    set w _ _    = w
    zero         = fromInteger 0

mkPermutationVector :: Int -> V.Vector Int
mkPermutationVector d = V.generate (2^d) b
  where
    b i | i < 2^(d-1) = 2*i
        | otherwise   = let i' = i-2^(d-1)
                        in 2*i'+1

permute :: Int -> BitVector -> BitVector
permute d v = V.backpermute v (mkPermutationVector d)