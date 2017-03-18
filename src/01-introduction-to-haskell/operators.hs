{--
   The mostly commons operators for basic logic in Haskell
--}


-- the declaration of function with multiple params
-- is done that way because the currying. Reducing
-- functions to just a dataflow. So do this is wrong:
-- and :: (Bool, Bool) -> Bool
-- This means that a function and receives a tuple (Bool, Bool) as its
-- unique argument and returns a Bool

-- btw, makes some sense on a structured language.
-- HASKELL IS NOT A STRUCTURED LANGUAGE!

-- :: DECLARATIONS
eq :: Integer -> Integer -> Bool
greater :: Integer -> Integer -> Bool
greater'eq :: Integer -> Integer -> Bool
less :: Integer -> Integer -> Bool
less'eq :: Integer -> Integer -> Bool
not'equal :: Bool -> Bool -> Bool
and :: Bool -> Bool -> Bool
or :: Bool -> Bool -> Bool
not' :: Bool -> Bool

-- :: DEFINITIONS
eq x y = x == y
greater x y = x > y
greater'eq x y = x >= y
less x y = x < y
less'eq x y = x <= y
not'equal x y = x /= y
and x y = x && y
or x y = x || y
not' x = not x
