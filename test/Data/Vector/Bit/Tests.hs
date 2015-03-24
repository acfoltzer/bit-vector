{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Data.Bits
import Data.Vector.Bit
import qualified Data.Vector.Unboxed as V

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.QuickCheck hiding ((.&.))

instance Arbitrary BitVector where
  arbitrary = (unpackInteger . getNonNegative) `fmap` arbitrary

prop_packUnpack :: Int -> Bool
prop_packUnpack x | x >= 0 = pack (unpack x) == x
                  | otherwise = True

prop_unpackPack :: [Bool] -> Bool
prop_unpackPack xs
    | length xs < 64 && (pack (V.fromList xs') :: Int) >= 0
    = unpack (pack (V.fromList xs') :: Int) == V.fromList xs'
    | otherwise = True
  where xs' = reverse . dropWhile not . reverse $ xs

prop_addCorrect :: Int -> Int -> Bool
prop_addCorrect x y = x + y == pack (unpack x + unpack y)

prop_multCorrect :: Int -> Int -> Bool
prop_multCorrect x y = x * y == pack (unpack x * unpack y)

prop_subCorrect :: Int -> Int -> Bool
prop_subCorrect x y | x >= y = x - y == pack (unpack x - unpack y)
                    | otherwise = True

prop_absSignum :: Int -> Bool
prop_absSignum x | x >= 0 = abs (unpack x) * signum (unpack x) == unpack x
                 | otherwise = True

prop_clearZero :: NonNegative Int -> Bool
prop_clearZero nn = clearBit zeroBits n ==~ zeroBits
  where n = getNonNegative nn

prop_setZero :: NonNegative Int -> Bool
prop_setZero nn = setBit zeroBits n ==~ bit n
  where n = getNonNegative nn

prop_testZero :: NonNegative Int -> Bool
prop_testZero nn = testBit (zeroBits :: BitVector) n == False
  where n = getNonNegative nn

prop_countZero :: Bool
prop_countZero = popCount (zeroBits :: BitVector) == 0

prop_complementInverse :: BitVector -> Bool
prop_complementInverse v = complement (complement v) ==~ v

prop_orSet :: BitVector -> NonNegative Int -> Bool
prop_orSet v nn = testBit (v .|. bit n) n
  where n = getNonNegative nn

prop_andTest :: BitVector -> NonNegative Int -> Bool
prop_andTest v nn = testBit v n == not (v .&. bit n ==~ zeroBits)
  where n = getNonNegative nn

prop_andClear :: BitVector -> NonNegative Int -> Bool
prop_andClear v nn = clearBit v n ==~ v' .&. complement mask
  where n          = getNonNegative nn
        (v', mask) = padMax v (bit n)

prop_xorZero :: BitVector -> Bool
prop_xorZero v = v `xor` v ==~ zeroBits

prop_xorFlip :: BitVector -> NonNegative Int -> Bool
prop_xorFlip v nn = testBit v' n == not (testBit v n)
  where v' = v `xor` bit n
        n  = getNonNegative nn

main = $(defaultMainGenerator)
