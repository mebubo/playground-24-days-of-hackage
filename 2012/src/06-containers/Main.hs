module Main where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Foldable (fold, traverse_)

type Person = String
type Colour = String

peopleFavColours :: M.Map Person (S.Set Colour)
peopleFavColours = M.fromList
    [ ("p1", S.fromList ["c1", "c2"])
    , ("p2", S.fromList ["c1", "c3", "c4"])
    ]

allFavColours :: S.Set Colour
allFavColours = fold peopleFavColours

main :: IO ()
main = do
    print allFavColours
    traverse_ print allFavColours
