{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens.TH (makeLenses)
import Snap.Core (writeText, route, Snap, getParam)
import Snap.Http.Server (httpServe, setPort, defaultConfig)
import Snap.Snaplet (Snaplet, SnapletInit, makeSnaplet, nestSnaplet, addRoutes, Handler, with, serveSnaplet)
import Snap.Snaplet.SqliteSimple (Sqlite, sqliteInit, query, Only(..), FromRow, fromRow, field)
import Data.Maybe (listToMaybe)
import Data.Text (Text)

hello :: Snap ()
hello = writeText "Hello, world!"

app :: Snap ()
app = route [("/hello", hello)]

data App = App { _db :: Snaplet Sqlite }

makeLenses ''App

initApp :: SnapletInit App App
initApp = makeSnaplet "myapp" "My application" Nothing $
  App <$> nestSnaplet "db" db sqliteInit
      <* addRoutes [("/hello/:id", helloDb)]

instance FromRow Text where
  fromRow = field

helloDb :: Handler App App ()
helloDb = do
  Just mUId <- getParam "id"
  userName <- with db $
        listToMaybe <$>
          query "SELECT name FROM users WHERE id = ?" (Only mUId)
  writeText $ maybe "User not found" id (userName)

main' :: IO ()
main' = httpServe config app
  where config = setPort 2009 mempty

main :: IO ()
main = serveSnaplet config initApp
  where config = setPort 2009 mempty
