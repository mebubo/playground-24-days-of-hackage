{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html.Renderer.Pretty (renderHtml)

type UserName = String

greet :: UserName -> Html
greet userName = H.docTypeHtml $ do
    H.head $
        H.title "Hello!"
    H.body $ do
        H.h1 "Tervetuloa!"
        H.p $ "Hello " >> H.toHtml userName >> "!"

addHr :: [Html] -> Html
addHr [] = mempty
addHr [p] = p
addHr (p:ps) = p >> H.hr >> addHr ps

doc :: Html
doc = H.docTypeHtml $
    H.body $
        addHr
            [ H.p "Hello, world!"
            , H.div "How are you?"]

main :: IO ()
main = do
    putStrLn $ renderHtml $ greet "Bob"
    putStrLn $ renderHtml $ doc
