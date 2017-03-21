{-- Exercises --}

-- Exercise 1
-- Write a recursive function copy :: [a] -> [a] that copies
-- its list argument. For example, copy [2] => 2.

copy :: [a] -> [a]
copy [] = []
copy (x:xs) = x : copy xs

-- Exercise 2
-- write a function inverse that takes a list of pairs and
-- swaps the pair elements. For example,
-- inverse [(1,2),(3,4)] => [(2,1),(4,3)]

inverse :: [(a,b)] -> [(b,a)]
inverse [] = []
inverse (x:xs) = swap x : inverse xs
    where swap (a,b) = (b,a)

-- Exercise 3
-- Write a function
merge :: Ord a => [a] -> [a] -> [a]
-- which takes two sorted lists and returns a sorted list
-- containing the elements of each
-- merge [1,2,3] [4,5,6] => [1,2,3,4,5,6]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x <= y = x:merge xs (y:ys)
    | otherwise = y:merge (x:xs) ys

-- Hand evaluation of merge
{--

merge [3,4] [5,6]
= 3 : (merge [4] [5,6])
= 3 : 4 : (merge [] [5,6])
= 3 : 4 : [5,6]
= [3,4,5,6]

--}

-- Exericse 4
-- Write (!!), a function that takes a natural number n
-- and a list and selects the nth element of the list. List elements
-- are indexed from 0, not 1, and since the type of the incoming number
-- does not prevent it from being out of range, the result should be a Maybe
-- type

nth :: (Integral n, Eq a) => [a] -> n -> Maybe a
nth [] _ = Nothing
nth (x:_) 0 = Just x
nth (_:xs) n = nth xs $ n - 1

-- Exercise 5
-- Write a function lookup that takes a value and a list
-- of pairs, and returns the second element of the pair that
-- has the value as its first element. Use a Maybe type
-- to indicate wheter the lookup succeded.
-- For example
--
-- lookup 5 [(1,2), (5,3)] => Just 3
-- lookup 6 [(1,2), (5,3)] => Nothing

lookup' :: (Eq a) => a -> [(a,b)] -> Maybe b
lookup' _ [] = Nothing
lookup' x ((a,b):xs)
    | x == a  = Just b
    | otherwise = lookup x xs

-- Exercise 6
-- Write a function that counts the number of times
-- an element appears in a list
count' :: (Eq a) => a -> [a] -> Int
count' _ [] = 0
count' e (x:xs)
    | e == x = 1 + count' e xs
    | otherwise = count' e xs

-- Exercise 7
-- Write a function that takes a value e and a list of
-- values xs and remove all occurrences of e from xs

remove :: (Eq a) => a -> [a] -> [a]
remove _ [] = []
remove e (x:xs)
    | e == x = remove e xs
    | otherwise = x : remove e xs


-- Exericse 8
-- Write a function
-- f :: [a] -> [a]
-- that removes alternating elements of its lists arguments,
-- starting with the first one. For example:
-- f [1,2,3,4,5,6,7] => [2,4,6]
alternate :: [a] -> [a]
alternate [] = []
alternate (_:y:xs) = y:alternate xs
alternate (_:xs) = xs

-- Exercise 9
-- Write a function
extract :: [Maybe a] -> [a]
-- that takes a list of Maybe values and return
-- the elements they contain. For example,
-- extract [Just 3, Nothing, Just 7]
extract [] = []
extract (Nothing:xs) = extract xs
extract (Just a:xs) = a : extract xs


-- Exercise 10
-- Write a function
-- f :: String -> String -> Maybe Int
-- that takes two strings. If the second string appears
-- within the first, it return the index identifying where
-- it starts. Indexes starts from 0. For example,
-- f "abcde" "bc" => Just 1
-- f "abcde" "fg" => Nothing


find :: String -> String -> Maybe Int
find _ [] = Nothing
find [] _ = Nothing
find str@(_:xs) substr
    | str `startsWith` substr = Just 0
    | otherwise = addMaybe $ find xs substr
    where addMaybe x = case x of Nothing -> Nothing
                                 Just v -> Just (v + 1)
          startsWith [] [] = True
          startsWith [] _ = False
          startsWith _ [] = True
          startsWith (a:as) (b:bs) = and $ (a == b):[startsWith as bs]

-- Exercise 11
-- Write foldrWith a function that behaves like foldr except
-- that it takes a function of three arguments and two lists
foldrWith :: (a -> b -> c -> c) -> c -> [a] -> [b] -> c
foldrWith _ z [] _ = z
foldrWith _ z _ [] = z
foldrWith f z (x:xs) (y:ys) = f x y $ foldrWith f z xs ys

-- Exercise 12
-- Using foldr, write a function mappend such that
-- mappend f xs = concat (map f xs)

mappend' :: ([a] -> [b]) -> [[a]] -> [b]
mappend' f xs = foldr ((++) . f) [] xs

-- Exercise 13
-- Write removeDuplicates, a function that takes a list and remove
-- all of its duplicate elements.

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x: removeDuplicates [k | k <- xs, x /= k]


-- Exercise 14
-- Write a recursive function that takes a value and a list of
-- of values and returns True if the value is in the list and
-- False otherwise.

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' e (x:xs)
    | e == x = True
    | otherwise = elem' e xs


-- Exercise 15
-- Write a function that takes two lists, and returns a list of values that appear in both lists.
-- The function should have type:
intersection :: Eq a => [a] -> [a] -> [a]
-- (This is one way to implement the intersection operation on sets; see Chapter 8)

intersection [] _ = []
intersection _ [] = []
intersection (x:xs) ys
    | x `elem` ys = x : intersection xs ys
    | otherwise = intersection xs ys


-- Exercise 16
-- Write a function that takes two lists, and returns True if all the elements
-- of the first list also occur in the other. The function should have
-- type:
isSubset :: Eq a => [a] -> [a] -> Bool
-- (This is one way to determine wheter one set is a subset of another; see Chapter 8)

isSubset xs ys = intersection xs ys == xs

-- Exercise 17
-- Write a recursive function that determines wheter a list is sorted
isSorted :: (Ord a) => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:xs) = x <= head xs && isSorted xs

-- Exercise 18
-- Show that the definition of factorial using foldr always produces
-- the same result as the recursive definition given in the previous section.

factorialr :: (Integral n) => n -> n
factorialr 0 = 1
factorialr n = n * factorialr(n - 1)


factorialf :: (Integral n) => n -> n
factorialf n = foldr (*) 1 [1..n]

{-- Hand evaluation of factorialr

factorialr 4
= 4 * factorialr 3
= 4 * (3 * factorialr 2)
= 4 * (3 * (2 * factorialr 1))
= 4 * (3 * (2 * (1 * factorialr 0)))
= 4 * (3 * (2 * (1 * 1)))
= 4 * (3 * 2)
= 4 * 6
= 24

--}

{-- Hand evaluation of factorialf
factorialf 4 <=> foldr (*) 1 [1,2,3,4]
= 1 * foldr (*) 1 [2,3,4]
= 1 * (2 * foldr (*) 1 [3,4])
= 1 * (2 * (3 * foldr (*) 1 [4]))
= 1 * (2 * (3 * (4 * foldr (*) 1 [])))
= 1 * (2 * (3 * (4 * 1)))
= 1 * (2 * 12)
= 24

--}


-- Exercise 19
-- Using recursion, define last, a function that takes a list and
-- returns a Maybe type that is Nothing if the list is empty

last' :: [a] -> Maybe a
last' [] = Nothing
last' [x] = Just x
last' (_:xs) = last' xs

-- Exercise 20
-- Using recursion, write two functions that expect a string containing
-- a number that contains a decimal point (for example, 23.455)
-- The first function returns the whole part of the number (i.e., the part
-- to the left of the decimal point). The second function returns the fractional part (the part of the right of the decimal point).

-- split :: String -> (String, String)


whole :: String -> String
whole [] = []
whole ('.':_) = []
whole (x:xs) = x : whole xs

fractional :: String -> String
fractional [] = []
fractional (_:'.':xs) = xs
fractional (x:xs) = fractional xs

-- test zone
testExercise :: (Integer, Bool) -> String
testExercise (n, solved) = "Exercise " ++ show n ++ ": "
                           ++ if solved then "Solved" else "Incorrect"



tests :: [(Integer, Bool)]
tests = [(1, [1,2,3] == copy [1,2,3]),
         (2, [(2,1),(3,2),(4,3)] == inverse [(1,2),(2,3),(3,4)]),
         (3, [1,2,3,3,4,5] == merge [1,2,3] [3,4,5]),
         (4, Just 5 == nth [1,2,3,4,5] 4),
         (5, Just 3 == lookup' 5 [(1,2), (5,3)]),
         (6, 3 == count' 3 [3,3,3]),
         (7, [1,2,3] == remove 4 [1,2,3,4,4,4]),
         (8, [2,4,6] == alternate [1,2,3,4,5,6,7]),
         (9, [3,7] == extract [Just 3, Nothing, Just 7]),
         (10, Just 0 == find  "manel" "man"),
         (11, 12 == foldrWith (\x y z -> x + y + z) 0 [1,2,3] [1,2,3]),
         (12, [1,2,3,4,5,6] == mappend' (map (+1)) [[0,1,2], [3,4,5]]),
         (13, [1,2,3] == removeDuplicates [1,1,2,2,3,3]),
         (14, True == elem' 1 [0,0,1]),
         (15, [1,2,3] == intersection [0,1,2,3,4] [-1,1,2,3,5]),
         (16, isSubset [1, 2, 3] [1, 2, 3, 4]),
         (17, isSorted [0, 1, 2, 3, 4, 5]),
         (18, factorialr 10 == factorialf 10),
         (19, Just 3 == last' [1,2,3]),
         (20, "321" == fractional "123.321" && "123" == whole "123.321")]

main :: IO()
main = mapM_ (putStrLn . testExercise) tests
