module Geometry.Cube
  ( volume
  , area) where

import qualified Geometry.Cuboid as Cuboid

volume :: (Floating a) => a -> a
volume a = Cuboid.volume a a a

area :: (Floating a) => a -> a
area a = Cuboid.area a a a
