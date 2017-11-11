{-# LANGUAGE RankNTypes #-}

module Main where

import Text.Parsec

type P a = forall u. Parsec String u a

countryCode :: P String
countryCode = count 2 upper

regCode :: P String
regCode = count 3 upperNum
    where upperNum = upper <|> digit

regYear :: P String
regYear = count 2 digit

recordingId :: P String
recordingId = count 5 digit

data ISRC = ISRC
    { isrcCountryCode :: String
    , isrcRegCode :: String
    , isrcRegYear :: Int
    , isrcRecording :: Int } deriving Show

isrcParser :: P ISRC
isrcParser = ISRC
    <$> countryCode
    <*> regCode
    <*> fmap read regYear
    <*> fmap read recordingId
    <* eof

isrcParser2 :: P [String]
isrcParser2 = sequence [ countryCode, regCode, regYear, recordingId ] <* eof

main :: IO ()
main = do
    print $ parse isrcParser "" "USPR37300012"
    print $ parse isrcParser "" "USPR37300012x"
    print $ parse isrcParser "" "USPr37300012"
    print $ parse isrcParser2 "" "USPR37300012"
    print $ parse isrcParser2 "" "USPR37300012x"
    print $ parse isrcParser2 "" "USPr37300012"
