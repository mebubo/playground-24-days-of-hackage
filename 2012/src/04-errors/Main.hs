{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Error (readMay, note, scriptIO, headMay, runScript)
import Control.Error.Util (hoistEither)
import Control.Monad.Trans.Except (ExceptT)
import System.Environment (getArgs)
import Data.Text (Text)

askAge :: IO (Either String Int)
askAge = note "Invalid input" . readMay <$> getLine

readPrintAge :: IO ()
readPrintAge = do
    putStr "Enger age: "
    age <- askAge
    print age

main :: IO ()
main = runScript $ do
    -- filename <- hoistEither =<< ((note usage . headMay) <$> (scriptIO getArgs))
    filename <- do
        a <- (note usage . headMay) <$> (scriptIO getArgs) :: ExceptT Text IO (Either Text FilePath)
        hoistEither a :: ExceptT Text IO FilePath
    -- scriptIO $ readFile filename >>= putStrLn . line5
    (scriptIO $ do
        b <- readFile filename :: IO String
        putStrLn $ line5 b :: IO ()) :: ExceptT Text IO ()
    where
        usage = "Usage: line5 <line>"
        line5 l = lines l !! 5
