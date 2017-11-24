{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans.Writer (WriterT, tell, runWriterT)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (listToMaybe)
import Data.Functor.Compose (Compose)
import System.Environment (getArgs)

type Filename = String
type User = String

listAllUsers :: Filename -> IO [User]
listAllUsers filename = lines <$> readFile filename

findUser :: Filename -> User -> IO (Maybe User)
findUser filename user = (listToMaybe . filter (==user)) <$> listAllUsers filename

listAllUsers' :: ReaderT Filename IO [User]
listAllUsers' = do
    filename <- ask
    lift $ listAllUsers filename

findUser' :: User -> ReaderT Filename IO (Maybe User)
findUser' user = do
    filename <- ask
    lift $ findUser filename user

listAllUsersLogged :: WriterT [String] (ReaderT Filename IO) [User]
listAllUsersLogged = do
    tell ["Listing all users..."]
    users <- lift listAllUsers'
    tell ["Found " ++ show (length users) ++ " users"]
    pure users

main :: IO ()
main = do
    [filename, user] <- getArgs
    (_, logs) <- flip runReaderT filename $ runWriterT $ do
        users <- listAllUsersLogged
        lift $ lift $ print users
        user <- lift $ findUser' user
        lift $ lift $ putStrLn $ maybe "Not found" ("User: " ++) user
    print logs

data MergeScope a = MergeScope
    { left :: a
    , original :: a
    , right :: a
    }

newtype Merge e a = Merge (Compose ((->) (MergeScope e)) Maybe a)
    deriving (Functor, Applicative)
