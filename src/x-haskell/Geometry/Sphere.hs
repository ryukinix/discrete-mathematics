module Geometry.Sphere
  ( volume
  , area
  ) where

volume :: (Floating r) => r -> r
volume r = (4/3) * pi * (r ** 3)

area :: (Floating r) => r -> r
area r = 4 * pi * (r ** 2)
