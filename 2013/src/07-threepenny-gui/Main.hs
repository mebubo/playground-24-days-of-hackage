module Main where

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Graphics.UI.Threepenny.Core (Window, UI, startGUI, defaultConfig, (#), set, attr,
    (#+), string, getBody, element, on, get, value, liftIO, children)
import qualified Graphics.UI.Threepenny as UI
import Control.Concurrent.STM (atomically, newTVar, TVar, readTVar, writeTVar, modifyTVar)
import Control.Monad (void)

type Database = Map UserName ToDoList
type UserName = String
type ToDoList = Set String

setup :: TVar Database -> Window -> UI ()
setup database rootWindow = void $ do
    userNameInput <- UI.input # set (attr "placeholder") "User name"
    loginButton <- UI.button #+ [ string "Login" ]
    getBody rootWindow #+ map element [ userNameInput, loginButton ]

    on UI.click loginButton $ \_ -> do
        userName <- get value userNameInput

        header <- UI.h1 #+ [ string $ userName ++ "'s To-Do List" ]

        currentItems <- fmap Set.toList $ liftIO $ atomically $ do
            db <- readTVar database
            case Map.lookup userName db of
                Nothing -> do
                    writeTVar database (Map.insert userName Set.empty db)
                    pure Set.empty
                Just items -> return items

        let showItem item = UI.li #+ [ string item ]
        toDoContainer <- UI.ul #+ map showItem currentItems

        newItem <- UI.input

        on UI.sendValue newItem $ \input -> do
            liftIO $ atomically $ modifyTVar database $
                Map.adjust (Set.insert input) userName

            set UI.value "" (element newItem)
            element toDoContainer #+ [ showItem input ]

        set children
            [ header, toDoContainer, newItem ]
            (getBody rootWindow)

main :: IO ()
main = do
    database <- atomically $ newTVar Map.empty
    startGUI defaultConfig (setup database)
