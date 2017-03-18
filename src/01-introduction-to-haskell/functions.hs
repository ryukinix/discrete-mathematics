{--

    This file explain the basic function to do:

    * type declaration
    * pattern matching
    * function definition
    * function composition

--}

-- :: factorial function
fat :: Integer -> Integer
fat 0 = 1
fat n | n > 0 = n * fat (n - 1)

-- :: fibonacci function

-- | domain and contra-domain declaration
fib :: Integer -> Integer
-- | pattern matching
fib n | n <= 1 = 1
-- | formal definition of fib function
fib n | n > 1 = fib(n - 1) + fib(n - 2)

-- | function composition
-- is just like fatfib = fat(fib(x))
fatfib :: Integer -> Integer
fatfib = fat . fib


-- :: Function types
-- f :: a -> b
-- The function argument has the type a and result the type b
-- For example, some of the Haskell functions that we have already seen have the following types:
-- sqrt :: Double -> Double
-- max :: Integer -> Integer -> Integer
-- not :: Bool -> Bool
-- toUpper :: Char -> Char

-- :: Operators and Functions
-- When we are dealing with operators, we just put parenthesis around it
-- to specify at it. Operators are just functions with infix arguments.
-- An another way to do 3+2 is calling (+) 3 2, the two statements will result
-- in 6.
-- We can use any function as operator just using a back-quote on them.
-- max 2 3 => 3 is equal to 2 `max` 3 => 3

m = max 2 3
m' = 2 `max` 3
m'' = m == m'

-- :: Function Definitions
-- We can define new functions giving the type declaration followed by the
-- defining equation.
-- function_name :: arg1Type -> arg2Type -> ... -> argTypen -> resultType
-- function_name arg1 arg2 arg3 = exp using the arguments

square :: Integer -> Integer
square x = x * x
-- square 2 => 4


-- :: Pattern Matching
-- If the argument on the left-hand side is a constant,
-- then the right-hand will be used only if the function is applied
-- to that value.
-- This makes it possible for a function definition to consist of several
-- defining equations.

f :: Integer -> String
f 1 = "one"
f 2 = "two"
f 3 = "three"
f n = show n

isThree :: Int -> Bool
isThree 3 = True
isThree x = False

nor :: Bool -> Bool -> Bool
nor False False = True
nor a b = False

first :: (a, b) -> a
first (x,y) = x

second :: (a,b) -> b
second (x,y) = y

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty (x:xs) = False

-- guards are indicated by pipes that follow a function's names
-- and its parameters. A more powerful way of pattern matching
-- using multiple conditions
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"
  | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pfft, i bet you are ugly!"
  | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"
  | otherwise                   = "You're a whale, congratulations!"

max' :: (Ord a) => a -> a -> a
max' a b
  | a > b     = a
  | otherwise = b


-- where bindings
initials :: String -> String -> String
initials firstname lastname = [f] ++ ".  " ++ [l] ++ "."
  where (f:_) = firstname
        (l:_) = lastname

-- let bindings
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
   in sideArea + 2 * topArea

-- case expressions
head' :: [a] -> a
head' xs = case xs of [] -> error "No head for empty lists!"
                      (x:_) -> x

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list"

-- alternative version using where

describeList' xs = "The list is " ++ what xs
  where what [] = "empty."
        what [x] = "a singleton"
        what xs = "a longer list"


f' x = case x of 10 -> 10
                 x -> x + 1
