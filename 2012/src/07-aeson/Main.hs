{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Map (Map)
import Data.Aeson (ToJSON(..), FromJSON(..), (.=), Value(..), object, (.:), encode,
    decode, eitherDecode)

data Cheese = Cheese
    { cheeseMaturity :: Maturity
    , cheeseWeight :: Double } deriving Show

data Maturity = Strong | Mild deriving Show

instance ToJSON Maturity where
    toJSON Strong = String "strong"
    toJSON Mild = String "mild"

instance FromJSON Maturity where
    parseJSON (String "strong") = pure Strong
    parseJSON (String "mild") = pure Mild
    parseJSON _ = fail "Unknown cheese strength"

instance ToJSON Cheese where
    toJSON Cheese{..} = object
        [ "maturity" .= cheeseMaturity
        , "weight" .= cheeseWeight
        ]

instance FromJSON Cheese where
    parseJSON (Object o) = Cheese <$> o .: "maturity" <*> o .: "weight"
    parseJSON _ = fail "Failed to parse cheese"

cheese :: Cheese
cheese = Cheese Strong 0.1

main :: IO ()
main = do
    print $ toJSON cheese
    print $ encode cheese
    print (decode "{\"maturity\": \"mild\", \"weight\": 0.2}" :: Maybe Cheese)
    print (eitherDecode "{\"weight\": 0.2}" :: Either String Cheese)
    print (eitherDecode "{\"maturity\": \"foo\", \"weight\": 0.2}" :: Either String Cheese)
    print (eitherDecode "{\"maturity\": \"mild\", \"weight\": []}" :: Either String Cheese)
    print (decode "{\"foo\":1,\"bar\":2}" :: Maybe (Map String Int))
