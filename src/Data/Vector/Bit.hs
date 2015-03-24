{- |

Module      :  Data.Vector.Bit
Description :  Simple bit vectors for Haskell
Copyright   :  (c) Adam C. Foltzer 2011-2015
License     :  BSD3

Maintainer  :  acfoltzer@gmail.com
Stability   :  experimental
Portability :  portable

-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Vector.Bit (
  -- * Bit vectors
  BitVector,

  -- * Conversions

  -- ** To and from other 'Bits' instances
  unpack, pack,

  -- ** Specialized conversions
  unpackInteger, packInteger, unpackInt, packInt,

  -- * Utilities
  pad, padMax, zipPad, trimLeading, (==~)
  )

where

import Data.Bits
import Data.Function
import qualified Data.Vector.Unboxed as V

-- | A 'BitVector' is a little-endian 'V.Vector' of
-- 'Bool's.
type BitVector = V.Vector Bool

-- | Pads a 'BitVector' to the specified length by adding a vector of
-- 'False' values to the most-significant end.
pad :: Int -> BitVector -> BitVector
pad i v = v V.++ V.replicate (i - V.length v) False

-- | Pads two 'BitVector's to the length of the longest vector. If the
-- vectors are the same length, 'padMax' does nothing.
padMax :: BitVector -> BitVector -> (BitVector, BitVector)
padMax xs ys = (padlen xs, padlen ys)
  where
    padlen = pad $ max (V.length xs) (V.length ys)

-- | Like 'V.zip', except pads the vectors to equal length
-- rather than discarding elements of the longer vector.
zipPad :: BitVector -> BitVector -> V.Vector (Bool, Bool)
zipPad xs ys = uncurry V.zip (padMax xs ys)

-- | Like 'V.zipWith', except pads the vectors to equal length
-- rather than discarding elements of the longer vector.
zipPadWith :: (Bool -> Bool -> Bool) -> BitVector -> BitVector -> BitVector
zipPadWith f xs ys = uncurry (V.zipWith f) (padMax xs ys)

-- | Discards any 'False' values at the most-significant end of the
-- given 'BitVector'.
trimLeading :: BitVector -> BitVector
trimLeading = V.reverse . V.dropWhile not . V.reverse

infix 4 ==~
-- | Equality modulo trailing 'False' bits
(==~) :: BitVector -> BitVector -> Bool
(==~) = (==) `on` trimLeading

instance Num BitVector where
  fromInteger = unpackInteger
  as + bs = if cout then V.tail sums `V.snoc` True else V.tail sums
    where
      cout            = V.last carries
      (sums, carries) = V.unzip sumsAndCarries
      sumsAndCarries  = V.scanl' fullAdd (False, False) (zipPad as bs)
      fullAdd (_, cin) (a, b) = ((a /= b) /= cin, (a && b) || (cin && (a /= b)))
  as * bs = trimLeading (sum partials)
    where
      partials = zipWith shiftMult (V.toList as) [0 ..]
      shiftMult True i  = bs `shiftL` i
      shiftMult False _ = V.empty
  as - bs = trimLeading $ V.take (V.length as') (rawSum + 1)
    where
      rawSum     = as' + complement bs'
      (as', bs') = padMax as bs
  abs = id
  signum v | V.null v  = 0
           | otherwise = 1

instance Bits BitVector where
  (.&.)        = zipPadWith (&&)

  (.|.)        = zipPadWith (||)

  xor          = zipPadWith (/=)

  complement   = V.map not

  shiftL v i   = V.replicate i False V.++ v

  shiftR       = flip V.drop

  rotateR v i  = high V.++ low
    where (low, high) = V.splitAt i v

  rotateL v i  = high V.++ low
    where (low, high) = V.splitAt (V.length v - i) v

  setBit v i | i < V.length v = V.unsafeUpd v [(i, True)]
             | otherwise      = V.generate (i+1) f
    where f j | i == j    = True
              | otherwise = testBit v j

  clearBit v i | i < V.length v = V.unsafeUpd v [(i, False)]
               | otherwise      = v

  testBit v i
    | Just b <- v V.!? i = b
    | otherwise          = False

  bit n = V.generate (n+1) (== n)

  bitSizeMaybe = Just . V.length

  bitSize = finiteBitSize

  isSigned = const False

  popCount = V.foldl' (\x b -> if b then x+1 else x) 0

instance FiniteBits BitVector where
  finiteBitSize = V.length

-- | Converts an instance of 'FiniteBits' to a 'BitVector'.
unpack :: (FiniteBits a) => a -> BitVector
unpack w = trimLeading $ V.generate (finiteBitSize w) (testBit w)

-- | Converts a 'BitVector' to an instance of 'Bits'.
pack :: (Num a, Bits a) => BitVector -> a
pack v = V.ifoldl' set 0 v
  where
    set w i True = w `setBit` i
    set w _ _    = w

unpackInteger :: Integer -> BitVector
unpackInteger = V.unfoldr f
  where
    f (flip divMod 2 -> (0, 0)) = Nothing
    f (flip divMod 2 -> (q, 0)) = Just (False, q)
    f (flip divMod 2 -> (q, 1)) = Just (True, q)
    f _                         = error "unexpected remainder when unpacking"

packInteger :: BitVector -> Integer
packInteger = pack

unpackInt :: Int -> BitVector
unpackInt = unpack

packInt :: BitVector -> Int
packInt = pack
