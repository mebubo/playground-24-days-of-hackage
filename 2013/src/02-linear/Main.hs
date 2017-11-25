{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Linear (norm, dot, Epsilon, M44, m33_to_m44, fromQuaternion, axisAngle,
    V3(V3), V4, translation, identity, (!*!))
import qualified Data.IntMap as IntMap
import Control.Arrow ((&&&))
import Control.Lens ((.~), (&))
import Control.Applicative (liftA2)

intMap :: IntMap.IntMap Float
intMap = IntMap.fromList $ map (id &&& fromIntegral) [0..10]

norm' :: Float
norm' = norm intMap

dot' :: Int
dot' = dot (IntMap.fromList [(0, 5), (1, 0)])
           (IntMap.fromList [(0, 1), (1, 1)])

triangleRotation :: (Epsilon a, Floating a) => a -> M44 a
triangleRotation t =
    m33_to_m44 $
        fromQuaternion $
            axisAngle (V3 0 1 0) (t * 2)

triangleTranslation :: forall a. Floating a => a -> M44 a
triangleTranslation t =
    (identity :: M44 a) & translation .~ V3 (sin t * 2) 0 (-5)

triangleTransformation :: (Epsilon a, Floating a) => a -> M44 a
triangleTransformation =
    liftA2 (!*!) triangleTranslation triangleRotation

a :: Int -> String
a n = replicate n 'a'

b :: Int -> String
b n = replicate n 'b'

ab :: Int -> String
ab = liftA2 (++) a b

main :: IO ()
main = do
    print norm'
    print dot'
    print $ ab 3
