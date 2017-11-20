module Main where

import Options.Applicative (execParser, argument, str, metavar,
    info, switch, short, long, help)
import Data.Char (toUpper)
import Data.Monoid ((<>))

data MyApp = MyApp
    { appGreet :: String
    , appSuperExcited :: Bool
    }

runWithOptions :: MyApp -> IO ()
runWithOptions opts = putStrLn
    $ transform
    $ "Merry Christmas, " ++ appGreet opts ++ "!"
    where
      transform = if appSuperExcited opts then map toUpper else id

main :: IO ()
main = execParser opts >>= runWithOptions
    where
        parser = MyApp
            <$> argument str (metavar "NAME")
            <*> switch (short 'e' <> long "excited" <> help "Run in excited mode")
        opts = info parser mempty
