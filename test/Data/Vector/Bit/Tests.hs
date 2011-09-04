import Data.Vector.Bit
import qualified Data.Vector.Unboxed as V
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2

prop_packUnpack :: Int -> Bool
prop_packUnpack x | x >= 0 = pack (unpack x) == x
                  | otherwise = True

prop_unpackPack :: [Bool] -> Bool
prop_unpackPack xs 
    | length xs < 64 && (pack (V.fromList xs') :: Int) >= 0 
    = unpack (pack (V.fromList xs') :: Int) == (V.fromList xs')
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
prop_absSignum x | x >= 0 = abs (unpack x) * signum (unpack x) == (unpack x)
                 | otherwise = True

main = defaultMain tests

tests = [ testGroup "Pack/Unpack" [
                          testProperty "pack . unpack" prop_packUnpack
                        , testProperty "unpack . pack" prop_unpackPack
                        ]
        , testGroup "Num instance" [
                          testProperty "+" prop_addCorrect
                        , testProperty "*" prop_multCorrect
                        , testProperty "-" prop_subCorrect
                        , testProperty "abs / signum" prop_absSignum
                        ]
        ]