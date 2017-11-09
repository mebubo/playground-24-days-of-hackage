{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens.TH (makeLenses)
import Control.Lens
import Numeric.Natural (Natural)
import Control.Lens.Prism (Prism')

data Point = Point
    { _x :: Double, _y :: Double } deriving Show

data Monster = Monster
    { _monsterLocation :: Point } deriving Show

makeLenses ''Point
makeLenses ''Monster

orge :: Monster
orge = Monster (Point 0 0)

nat :: Prism' Integer Natural
nat = prism toInteger $ \ i ->
    if i < 0
    then Left i
    else Right (fromInteger i)

main :: IO ()
main = do
    print $ monsterLocation.x +~ 1 $ orge
    print $ 5 ^? nat
    print $ (-5) ^? nat
    print $ both.nat *~ 2 $ (-3, 4)
    print $ both.nat *~ 2 $ (8, 4)
