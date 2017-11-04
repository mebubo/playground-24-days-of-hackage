{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text, null)
import Text.Digestive (Form, text, (.:), check, validateM, Result(Success, Error), View)
import Text.Digestive.Blaze.Html5 (form, label, inputText, inputTextArea, inputSubmit, errorList)
import Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import Snap.Core (Snap, writeText, route)
import Snap.Http.Server (httpServe, setPort)
import Snap.Blaze (blaze)
import Text.Digestive.Snap (runForm)

data Category = Category { catName :: Text }

data BlogPost = BlogPost
    { postTitle :: Text
    , postBody :: Text
    , postCategory :: Category
    }

postForm :: Monad m => Form Html m BlogPost
postForm = BlogPost
    <$> "title" .: text Nothing
    <*> "body" .: text Nothing
    <*> (Category <$> "category" .: text Nothing)

postForm' :: Monad m => Form Html m BlogPost
postForm' = BlogPost
    <$> "title" .: nonEmptyText
    <*> "body" .: nonEmptyText
    <*> (Category <$> "category" .: nonEmptyText)
    where
        nonEmptyText = check "Cannot be empty" (not . Data.Text.null) $ text Nothing

type BlogWebsite = Snap

lookupCategory :: Text -> BlogWebsite (Maybe Category)
lookupCategory c = pure $ if c `elem` cats then Just $ Category c else Nothing
    where cats = ["cat1", "cat2"]

postForm'' :: Form Html BlogWebsite BlogPost
postForm'' = BlogPost
    <$> "title" .: nonEmptyText
    <*> "body" .: nonEmptyText
    <*> "category" .: category
    where
        nonEmptyText = check "Cannot be empty" (not . Data.Text.null) $ text Nothing
        category = validateM ((maybe failure Success <$>) . lookupCategory) nonEmptyText
        failure = Error "Category does not exist"

renderForm :: View Html -> Html
renderForm v = do
  form v "/form" $ do
    H.p $ do
      errorList "title" v
      label "title" v "Post title: "
      inputText "title" v
    H.p $ do
      errorList "body" v
      label "body" v "Post: "
      inputTextArea Nothing Nothing "body" v
    H.p $ do
      errorList "category" v
      label "category" v "Category: "
      inputText "category" v
    inputSubmit "Submit Post"

hello :: Snap ()
hello = writeText "Hello"

blogPostHandler :: Snap ()
blogPostHandler = do
  (view, result) <- runForm "blog-post" postForm''
  case result of
    _ -> blaze $ renderForm view
    -- Nothing -> blaze $ renderForm view
    -- Just blogPost -> do
    --   with blogSite $ addPost blogPost
    --   redirect (postUrl blogPost)

app :: Snap ()
app = route
  [ ("/hello", hello)
  , ("/form", blogPostHandler)
  ]

main :: IO ()
main = httpServe config app
  where config = setPort 2009 mempty
