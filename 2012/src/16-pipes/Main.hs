module Main where

import Pipes
import Pipes.Core
import qualified Pipes.Prelude as P
import System.IO (isEOF)
import Control.Monad (unless, replicateM_)
import Control.Exception (try, throwIO)
import qualified GHC.IO.Exception as G


stdinLn :: Producer String IO ()
stdinLn = do
    eof <- lift isEOF
    unless eof $ do
        str <- lift getLine
        yield str
        stdinLn

process :: String -> Effect IO ()
process s = lift $ putStrLn $ ">>" ++ s ++ "<<"

processStdIn :: Effect IO ()
processStdIn = for stdinLn process

triple :: Monad m => a -> Producer a m ()
triple a = do
   yield a
   yield a
   yield a

loop :: Producer String IO ()
loop = for stdinLn triple

stdoutLn :: Consumer String IO ()
stdoutLn = do
    l <- await
    x <- lift (try $ putStrLn l :: IO (Either G.IOError ()))
    case x of
      Left e@(G.IOError { G.ioe_type = t }) ->
         lift $ unless (t == G.ResourceVanished) $ throwIO e
      Right () -> stdoutLn

doubleUp :: Monad m => Consumer String m String
doubleUp = do
   a <- await
   b <- await
   return $ a ++ b

take' :: Int -> Pipe a a IO ()
take' n = do
   replicateM_ n $ do
      x <- await
      yield x
   lift $ putStrLn "You shall not pass!"

maxInput :: Int -> Producer String IO ()
maxInput n = P.stdinLn >-> take' n

name :: Producer String IO ()
name = do
    lift $ putStr "Hohoho! What is your name? "
    l <- lift getLine
    yield l

data Present = Present String deriving Show

presents :: Producer Present IO ()
presents = do
   lift $ putStrLn "And what presents would you like?"
   P.stdinLn >-> P.takeWhile (not . null) >-> P.map Present

main :: IO ()
main = do
    runEffect $ for stdinLn $ lift . putStrLn
    runEffect $ for loop process
    runEffect $ for P.stdinLn (triple ~> lift . putStrLn)
    runEffect $ lift getLine >~ stdoutLn
    runEffect $ lift getLine >~ doubleUp >~ stdoutLn
    runEffect $ maxInput 3 >-> P.stdoutLn
    runEffect $ name >-> P.stdoutLn
    runEffect $ presents >-> P.map show >-> P.stdoutLn
