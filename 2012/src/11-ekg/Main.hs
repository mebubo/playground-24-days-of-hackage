{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forever)
import System.Remote.Monitoring (forkServer, getCounter)
import System.Metrics.Counter (Counter, inc, read)
import System.IO (hFlush, stdout)

main' :: Counter -> IO ()
main' counter = forever $ do
    putStr "> "
    hFlush stdout
    l <- getLine
    inc counter
    c <- System.Metrics.Counter.read counter
    putStrLn $ show c ++ " " ++ l


main :: IO ()
main = do
    ekg <- forkServer "0.0.0.0" 2000
    getCounter "lines" ekg >>= main'
