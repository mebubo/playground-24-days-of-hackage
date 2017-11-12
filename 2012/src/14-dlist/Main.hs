module Main where

import Data.DList (DList, fromList, toList)
import Data.Monoid ((<>))

l1 :: DList Int
l1 = fromList [1, 2, 3]

l2 :: DList Int
l2 = fromList [10..20]

main :: IO ()
main = do
    print $ toList $ l1 <> l2
