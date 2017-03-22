module Geometry.Cuboid
  ( volume
  , area
  ) where

volume :: (Fractional a) => a -> a -> a -> a
volume a b c = rectangleArea a (b * c)

area :: (Fractional a) => a -> a -> a -> a
area a b c =   2 * rectangleArea a b
             + 2 * rectangleArea b c
             + 2 * rectangleArea a c

rectangleArea :: (Fractional a) => a -> a -> a
rectangleArea a b = a * b
