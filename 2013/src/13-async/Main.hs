module Main where

import Control.Concurrent.Async
import Control.Concurrent (threadDelay)
import GHC.Exception (SomeException)

longSearch :: Int -> IO String
longSearch n = do
    threadDelay $ n * 1000000
    return "found"

failingSearch :: Int -> IO String
failingSearch n = do
    threadDelay $ n * 1000000
    undefined

startSearch :: IO (Async String)
startSearch = do
    putStrLn "start"
    search <- async $ longSearch 2
    putStrLn "stop"
    return search

startFailingSearch :: IO (Async String)
startFailingSearch = do
    putStrLn "start failing"
    search <- async $ failingSearch 2
    putStrLn "stop failing"
    return search

waitExample :: IO ()
waitExample = do
    search <- startSearch
    searchResults <- wait search
    putStrLn searchResults

pollExample :: IO (Async String) -> IO (Either SomeException String)
pollExample start = do
    search <- start
    let loop = do
            maybeResults <- poll search
            case maybeResults of
                Nothing -> do
                    putStrLn "Still searching"
                    threadDelay 300000
                    loop
                Just r -> return r
    loop

mapExample :: IO [String]
mapExample = mapConcurrently longSearch [1, 1, 1]

main :: IO ()
main = do
    waitExample
    print =<< pollExample startSearch
    print =<< pollExample startFailingSearch
    print =<< mapExample
