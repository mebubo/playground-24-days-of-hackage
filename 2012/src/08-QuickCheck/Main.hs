module Main where

import Test.QuickCheck (Property, quickCheck, (==>))

absAverage :: [Double] -> Double
absAverage ds = sum (abs <$> ds) / fromIntegral (length ds)

prop_nonNegative :: [Double] -> Property
prop_nonNegative ds = length ds > 0 ==> absAverage ds >= 0

main :: IO ()
main = quickCheck $ prop_nonNegative
