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

-- Exercise 9
-- Write a function that takes two lists of type [Maybe Int] and
-- examines the pair of list heads before looking at the rest of the lists.
-- It returns a list in which the Ints of each pair have been added if both
-- are of the form Just n, preserving any Just n value otherwise. For example,
-- addJust [Just 2, Nothing, Just 3] [Nothing, Nothing, Just 5]
--      => [Just 2, Nothing, Just 8]

-- using zipWith
addJust :: [Maybe Int] -> [Maybe Int] -> [Maybe Int]
addJust u v = zipWith maybeSum u v
  where maybeSum Nothing y = y
        maybeSum x Nothing = x
        maybeSum (Just x) (Just y) = Just $ x + y

-- using recursion
addJust' :: [Maybe Int] -> [Maybe Int] -> [Maybe Int]
addJust' [] _ = []
addJust' _ [] = []
addJust' (x:xs) (Nothing:ys) = x : addJust' xs ys
addJust' (Nothing:xs) (y:ys) = y : addJust' xs ys
addJust' ((Just x):xs) ((Just y):ys) = Just (x + y) : addJust' xs ys


-- Exercise 10
-- Define a data type that represents six different metals and automatically
-- creates versions of (==) and show

data Metal = Gold | Steel | Bronze | Iron
            | Silver | Nickel
            deriving (Show, Eq)

-- Exercise 11
-- Suppose that you have some coins that have been sorted
-- into piles, each of which contains only one kind of coin.
-- Define a data type that can be used to present the pile
-- of coins.
data Coin n = US n | BRL n | EUR n | JPZ n
             deriving (Show, Eq)

data Pile list = Coins list
                 deriving Show


bucket :: Pile [Coin Integer]
bucket = Coins $ [BRL x | x <- [1,2,5,10,25,50,100]]
-- => Coins [BRL 1,BRL 2,BRL 5,BRL 10,BRL 25,BRL 50,BRL 100]

-- Exercise 12
-- A universal type is one in which any type can be represented.
-- Each different type is identified by its own constructor, which
-- serves as distinguishing tag. For example, here is a universal type
-- that represents three different types of number.
--
-- data Number = INT Int | INTEGER Integer | FLOAT Float
--               deriving (Eq, Show)

data Universal = INT Int | INTEGER Integer | CHAR Char | BOOL Bool
                 deriving (Eq, Ord, Show)

-- Exercise 13
-- Define a type that contains tuples of up to four elements

data Tuple4 = T1 (Int) | T2 (Int, Int) | T3 (Int, Int, Int)
             | T4 (Int, Int, Int, Int)
             deriving (Eq, Show)

-- Exercise 14
-- The quadratic equation ax² + bx² + c = 0 has two roots given
-- by the formula ... as long as the discriminant is non-negative.
-- If the discriminant is negative the roots are complex.
-- Define a function that finds the real solutions of the quadratic
-- equation, and reports failure if they don't exists.

-- Floating because sqrt only returns a float
quadratic :: (Floating a, Ord a) => a -> a -> a -> Maybe (a, a)
quadratic a b c
  | discriminant < 0 = Nothing
  | a == 0 = Nothing
  | otherwise = let x1 = ((-b) + sqrt discriminant)/(2*a)
                    x2 = ((-b) - sqrt discriminant)/(2*a)
                    in Just (x1,x2)
    where discriminant = (b ^ 2) - 4 * a * c


-- Exercise 15
-- Define a function
-- showMaybe :: Show a => Maybe a -> String
-- that takes a Maybe value and prints it

showMaybe :: Show a => Maybe a -> String
showMaybe (Just x) = show x
showMaybe x = show x

-- Exercise 16
-- A bit is an integer that is either 0 or 1. A Word is a list of bits
-- that represents a binary number. Here are some binary values that can be
-- represented by words
--
-- [1, 0] => 2
-- [1,0,0,1] => 9
-- [1,1,1] => 7
--
-- We can define functions that are the Bit equivalent of or and and as
-- follows:
--
bitOr :: Int -> Int -> Int
bitOr 0 0 = 0
bitOr x y = 1

bitAnd :: Int -> Int -> Int
bitAnd 1 1 = 1
bitAnd x y = 0

-- Now it is possible to take the `bitwise` and of two words as follows:
-- bitwiseAnd [1,0,0] [1,0,0]
-- [bitAnd 1 1, bitAnd 0 0, bitAnd 0 1]
-- [1,0,0]

-- bitwiseAnd [0,0,0] [1,1,0]
-- [0,0,0]

bitwiseAnd :: [Int] -> [Int] -> [Int]
bitwiseAnd x y = zipWith bitAnd x y
bitwiseOr :: [Int] -> [Int] -> [Int]
bitwiseOr x y = zipWith bitOr x y

-- Exercise 17
-- Each of the following expressions has a type error. Change the expression so that
-- the type error no longer occurs.
-- [1, False]     '2' ++ 'a'
-- [(3, True), (False, 9)]   2 == False
-- 'a' > "b"              [[1], [2], [[3]]]
ex17_1 = (1, False)
ex17_2 = "2" ++ "a"
ex17_3 = [(3, True), (9, False)]
ex17_4 = 2 == 0
ex17_5 = 'a' > 'b'
ex17_6 = [[1],[2],[3]]

-- Exercise 18
-- What caused the type error in this definition and application
-- f :: Num a => (a, a) -> a
-- f (x,y) = x + y
-- f (True,4)

-- The type declaration assumes the both of elements of 2-uple must
-- be in the Type class Num, however, a fun call is made with a
-- heterogeneous tuple with a Bool type on the first element.
-- Calling f with any (x,y) where x and y are Nums will not raise a type error

-- Exercise 19
-- Why does this definition produce an error when used?
-- f :: Maybe a -> [a]
-- f Nothing = []
-- f (Just 3)

-- A non-exhaustive pattern matching is defined and called.
-- A f definition need be made to handle the (Just n) pattern.
-- This can be fixed that way: f (Just n) = n


-- Exercise 20
-- Write a list comprehension that takes a list of Maybe values
-- and returns a list of the Just Constructor Arguments. For example,
-- [Just 3, Nothing, Just 4] => [3, 4]

filterMaybe :: [Maybe Int] -> [Int]
filterMaybe list = map (\(Just n) -> n) $ filter (/= Nothing) list

filterMaybe' :: [Maybe Int] -> [Int]
filterMaybe' list = [n x | x <- list, x /= Nothing, let n (Just v) = v]


-- Exercise 21
-- Using a list comprehension, write a function that takes a list
-- of Int values and an Int value and returns those elements in the list
-- that are greater than n
filterGreaterThanN :: [Int] -> Int -> [Int]
filterGreaterThanN list n = [x | x <- list, x > n]


-- Exercise 22
-- Write a Function
f :: [Int] -> Int -> [Int]
-- That takes a list of Int values and an Int and return a list of indexes at
-- which that Int appears
f list n = [i | (x,i) <- zip list [0..], x == n]


-- Exercise 23
-- Write a list comprehension that produces a list giving all of the
-- positive integers that are not squares in the range 1 to 20

squaresFiltered = [x | x <- [1..], not $ x^2 `elem` [1..20]]
-- Be careful, infinite list.


-- Exercise 24
-- Write a function that uses foldr to count the number of times
-- a letter occurs in a string
-- count :: String -> Char -> Int
count :: (Foldable t, Num a1, Eq a) => t a -> a -> a1
count l x = foldr (\y acc -> if x == y then acc + 1 else 0) 0 l


-- Exercise 25
-- Write a function using foldr that takes a list and removes
-- each instance of a given letter
remove :: Foldable t => t Char -> Char -> [Char]
remove l x = foldr (\y acc -> if x == y then acc else y:acc) "" l

-- Exercise 26
-- Using foldl, write a function
rev :: [a] -> [a]
-- that reverses its list argument
rev l = foldl (\acc x -> x:acc) [] l


-- Exercise 27
-- Using foldl, write a function
maybeLast :: [a] -> Maybe a
-- That takes a list and returns the last element in it if there is one,
-- otherwise it returns Nothing

maybeLast l = foldl (\acc x -> Just x) Nothing l
