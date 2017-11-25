module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, readChan, newChan, writeChan)
import Control.Monad (forever)
import Data.Monoid (Sum(Sum), Product(Product))
import System.Environment (lookupEnv)

echo :: Chan String -> IO ()
echo c = forever $ do
    x <- readChan c
    putStrLn x

stats :: [Int] -> (Sum Int, Product Int)
stats = mconcat . map (\x -> (Sum x, Product x))

main :: IO ()
main = do
    print $ stats [1, 2, 3, 4]

    lookupEnv "HOME" >>= traverse putStrLn
    do
        h <- lookupEnv "HOME"
        traverse putStrLn h :: IO (Maybe ())

    c <- newChan
    forkIO (echo c)
    forever $ do
        l <- getLine
        writeChan c l
