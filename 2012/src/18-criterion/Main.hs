module Main where

import Criterion.Main (defaultMainWith, defaultConfig, bench, nf)

fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n - 1)

main :: IO ()
main = defaultMainWith
  defaultConfig
  [ bench "fact 30" $ nf fact 30
  , bench "fact 40" $ nf fact 40
  ]
