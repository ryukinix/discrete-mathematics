{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- example given on
-- https://bartoszmilewski.com/2014/12/05/categories-great-and-small/

class Monoid' m where
  mempty' :: m
  mappend' :: m -> m -> m


instance Monoid' String where
  mempty' = ""
  mappend' = (++)

main = print $ mappend' "Hello, " "Monoid!"
