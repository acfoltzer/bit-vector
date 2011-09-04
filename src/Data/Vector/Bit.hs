{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Vector.Bit where

import Data.Bits
import qualified Data.Vector.Unboxed as V

type BitVector = V.Vector Bool

zipPad xs ys = uncurry V.zip (padMax xs ys)

pad v i = v V.++ V.replicate (i - V.length v) False

padMax xs ys = (pad xs len, pad ys len)
  where
    len = max (V.length xs) (V.length ys)

trimLeading = V.reverse . V.dropWhile not . V.reverse

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
      partials = zipWith shiftMult (V.toList as) $ [0..]
      shiftMult True i  = bs `shiftL` i
      shiftMult False i = V.empty
  as - bs = trimLeading $ V.take (V.length as') (rawSum + 1)
    where
      rawSum     = as' + (complement bs')     
      (as', bs') = padMax as bs
  abs = id
  signum v | V.null v  = 0
           | otherwise = 1

instance Bits BitVector where
  (.&.)       = V.zipWith (&&)
  (.|.)       = V.zipWith (||)
  xor         = V.zipWith (/=)
  complement  = V.map not
  shiftL v i  = V.replicate i False V.++ v
  shiftR      = flip V.drop
  rotateR v i = high V.++ low
    where (low, high) = V.splitAt i v
  rotateL v i = high V.++ low
    where (low, high) = V.splitAt (V.length v - i) v
  bitSize     = V.length
  isSigned    = const False

unpack :: (Bits a) => a -> BitVector
unpack w = trimLeading $ V.generate (bitSize w) (testBit w)

pack :: (Bits a) => BitVector -> a
pack v = V.ifoldl' set zero v
  where
    set w i True = w `setBit` i
    set w _ _    = w
    zero         = fromInteger 0

unpackInteger :: Integer -> BitVector
unpackInteger = V.unfoldr f
  where
    f (flip divMod 2 -> (0,0)) = Nothing
    f (flip divMod 2 -> (q,0)) = Just (False, q)
    f (flip divMod 2 -> (q,1)) = Just (True, q)

unpackInt = unpack :: Int -> BitVector
packInt = pack :: BitVector -> Int