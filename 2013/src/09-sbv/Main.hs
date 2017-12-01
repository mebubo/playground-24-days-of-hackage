{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.SBV

p1 = prove . forAll ["x"] $ \ (x::SWord8) -> x * 2 .== x + x
p2 = prove . forAll ["x"] $ \ (x::SWord8) -> x * 3 .== x + x

s1 = sat . forSome ["x", "y"] $ \ (x::SInteger) y ->
    x^2 + y^2 .== 25 &&& 3 * x + 4 * y .== 0
s2 = sat . forSome ["x", "y"] $ \ (x::SInteger) y ->
    x^2 + y^2 .== 42 &&& 3 * x + 4 * y .== 0
s3 = sat . forSome ["x", "y", "z"] $ \ (x::SInteger) y z ->
    x + 2 * y .== 3 * y + z + x^2 &&& x ./= y

main :: IO ()
main = do
    print =<< p1
    p1' <- p1
    print (extractModel p1' :: Maybe Word8)

    print =<< p2
    p2' <- p2
    print (extractModel p2' :: Maybe Word8)

    print =<< s1
    s1' <- s1
    print (extractModel s1' :: Maybe [Integer])

    print =<< s2

    print =<< s3
