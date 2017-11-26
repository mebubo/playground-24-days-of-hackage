{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Network.HTTP.Types.Status (internalServerError500, badRequest400)

registerInterest :: String -> IO (Maybe String)
registerInterest "a@b.com" = putStrLn "Registered!" >> pure Nothing
registerInterest _ = pure $ Just "I broke :("

christmasly :: ScottyM ()
christmasly = do
    get "/" showLandingPage
    post "/register" register
    post "/register" registrationFailure

showLandingPage :: ActionM ()
showLandingPage = do
    setHeader "Content-Type" "text/html"
    file "landing.html"

register :: ActionM ()
register = do
    emailAddress <- param "email" `rescue` const next
    registered <- liftIO (registerInterest emailAddress)
    case registered of
        Just errorMessage -> do
            json $ object [ "error" .= errorMessage ]
            status internalServerError500

        Nothing ->
            json $ object [ "ok" .= ("ok" :: String) ]

registrationFailure :: ActionM ()
registrationFailure = do
    json $ object [ "error" .= ("Invalid request" :: String) ]
    status badRequest400

main :: IO ()
main = scotty 2001 christmasly
