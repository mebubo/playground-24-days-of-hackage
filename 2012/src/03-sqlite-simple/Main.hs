{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.SQLite.Simple (FromRow, fromRow, ToRow, toRow, field, Connection, query_, open, close, execute, execute_, Only(..))
import Database.SQLite.Simple.ToField (ToField, toField)
import Data.Text (Text)
import Control.Applicative (liftA2)

data Present = Present { presentName :: Text }

data Location = Location
    { locLat :: Double
    , locLon :: Double
    } deriving Show

data Child = Child
    { childName :: Text
    , childLocation :: Location
    } deriving Show

instance FromRow Present where
    fromRow = Present <$> field

instance FromRow Child where
    fromRow = Child <$> field <*> liftA2 Location field field

instance ToField Present where
    toField = toField . presentName

instance ToRow Child where
    toRow (Child name (Location lat lon)) = toRow (name, lat, lon)

allChildren :: Connection -> IO [Child]
allChildren c = query_ c "SELECT name, loc_lat, loc_long FROM child"

allPresents :: Connection -> IO [Present]
allPresents c = query_ c "SELECT name from present"

main :: IO ()
main = do
    conn <- open "test.db"
    execute_ conn "CREATE TABLE present(name TEXT)"
    execute_ conn "CREATE TABLE child(name TEXT, loc_lat REAL, loc_long REAL)"
    execute conn "INSERT INTO present VALUES (?)" $ Only $ Present "p1"
    execute conn "INSERT INTO present VALUES (?)" $ Only $ Present "p2"
    execute conn "INSERT INTO child VALUES (?, ?, ?)" $ Child "c1" $ Location 1 1
    execute conn "INSERT INTO child VALUES (?, ?, ?)" $ Child "c2" $ Location 2 2
    r <- query_ conn "SELECT * from child" :: IO [Child]
    mapM_ print r
    close conn
