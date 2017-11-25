module Main where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.SmallCheck (testProperty, (==>))
import Test.Tasty.HUnit (testCase, (@?=))
import Data.List (sort)

main :: IO ()
main = defaultMain $
    testGroup "Tests"
        [ testGroup "(checked by SmallCheck)"
            [ testProperty "sort == sort . reverse" $
                \list -> sort (list :: [Int]) == sort (reverse list)

            , testProperty "Fermat's last theorem" $
                \x y z n -> (n :: Integer) >= 3 ==>
                    x^n + y^n /= (z^n :: Integer)
            ]
        , testGroup "Unit tests"
            [ testCase "List comparison (different length)" $
                [1, 2, 3] `compare` [1, 2] @?= GT
            , testCase "List comparison (same length)" $
                [1, 2, 3] `compare` [1, 2, 2] @?= LT
            ]
        ]
