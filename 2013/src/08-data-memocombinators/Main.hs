module Main where

import qualified Data.MemoCombinators as Memo
import Data.Time (getCurrentTime, diffUTCTime, NominalDiffTime)

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibFast :: Int -> Int
fibFast = Memo.integral fib'
    where fib' 0 = 1
          fib' 1 = 1
          fib' n = fibFast (n - 1) + fibFast (n - 2)

timed :: IO () -> IO NominalDiffTime
timed a = do
    start <- getCurrentTime
    a
    end <- getCurrentTime
    return $ diffUTCTime end start

printTimed :: IO () -> IO ()
printTimed a = putStrLn . ("Took " ++) . show =<< timed a

main :: IO ()
main = do
    printTimed $ print $ fib 40
    printTimed $ print $ fibFast 1000
