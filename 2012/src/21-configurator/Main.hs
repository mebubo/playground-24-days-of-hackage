{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Configurator as C
import Control.Monad (forever)
import Control.Concurrent (threadDelay)

printUser :: Maybe String -> IO ()
printUser(Just u) = putStrLn u
printUser Nothing = putStrLn "Not found"

nonWorkingAutoReload :: IO ()
nonWorkingAutoReload = do
    (config', _) <- autoReload autoConfig [ Required "app.cfg"  ]
    user <- C.lookup config' "database.username"
    forever $ do
        printUser user
        threadDelay 1000000

workingAutoReload :: IO ()
workingAutoReload = do
    (config', _) <- autoReload autoConfig [ Required "app.cfg"  ]
    let userIO = C.lookup config' "database.username"
    forever $ do
        user <- userIO
        printUser user
        threadDelay 1000000

main :: IO ()
main = do
    config <- load [ Required "app.cfg"  ]
    user <- C.lookup config "database.username"
    printUser user
    workingAutoReload
