{-- A Compilation of the Exercises of Chapter 1

    INTRODUCTION TO HASKELL

--}


{-- Operators, Functions and basic Types (Lists, Strings, Int, Bool, Tuple)  --}
-- Exercise 3.
-- Write a function that takes a character and returns True
-- if the character is 'a' and False otherwise

ex3 :: Char -> Bool
ex3 'a' = True
ex3 c = False

-- Exercise 4.
-- Write a function that takes a string and returns True if
-- the string is "hello" and false otherwise. This can be done
-- by specifying each element of the string in the list pattern (e.g. 'h':'i':[])

ex4 :: String -> Bool
ex4 "hello" = True
ex4 s = False

-- Exercise 5.
-- Write a function that takes a string and removes a leading space if it exists.

ex5 :: String -> String

ex5 (' ':x) = x
ex5 x = x


-- Exercise 6
-- Suppose a program has read in a list of numbers of type Int.
-- Each number is intended to represent a Boolean value, where 0
-- Means False, 1 means True and any other number constitutes
-- invalid input. Write a function convert :: [Int] -> [Bool]
-- that converts a list of numbers to the corresponding Booleans
convert :: [Int] -> [Bool]
convert list = map bool list
  where bool n = case n of 0 -> False
                           1 -> True

-- Exercise 7
-- Write a function member0 :: String -> Bool that takes a list
-- of Char values (i.e. a String), and returns True if at least one
-- of the characters is '0' and False otherwise.

member0 :: String -> Bool
member0 s = or $ map (== '0') s

-- Exercise 8
-- Expand the following application
-- foldr max 0 [1,5,3]

-- Answer: max 1 (max 5 (max 3 0))
