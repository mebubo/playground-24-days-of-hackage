{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.Persist.TH
import Database.Persist (insert, insertMany)
import Database.Persist.Sqlite (runSqlite, runMigration)
import Database.Esqueleto (select, from, asc, limit, orderBy, (^.), InnerJoin(..),
    on, (==.))
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Control.Monad.IO.Class (liftIO)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name Text
    age Int Maybe
    deriving Show

StatusUpdate
    producer PersonId
    update Text
    createdAt UTCTime
    mood Text Maybe
    deriving Show
|]

sortedNames = select $ from $ \person -> do
    orderBy [asc (person ^. PersonName)]
    limit 5
    return $ person ^. PersonName

latestUpdates = select $ from $ \(person `InnerJoin` update) -> do
    on (person ^. PersonId ==. update ^. StatusUpdateProducer)
    orderBy [asc (update ^. StatusUpdateCreatedAt)]
    limit 5
    return (person ^. PersonName, update ^. StatusUpdateUpdate)

main :: IO ()
main = runSqlite "data/persistent.db" $ do
    runMigration migrateAll
    ollie <- insert (Person "Oliver Charles" $ Just 30)
    now <- liftIO getCurrentTime
    insertMany
        [ StatusUpdate ollie "Writing another blog post!" now Nothing
        , StatusUpdate ollie "I <3 24 days of Hackage" now (Just "^.^")
        ]
    sortedNames >>= mapM_ (liftIO . print)
    latestUpdates >>= mapM_ (liftIO . print)
