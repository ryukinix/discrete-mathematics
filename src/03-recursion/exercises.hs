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
    where swap (x, y) = (y,x)

-- Exercise 3
-- Write a function
merge :: Ord a => [a] -> [a] -> [a]
-- which takes two sorted lists and returns a sorted list
-- containing the elements of each
-- merge [1,2,3] [4,5,6] => [1,2,3,4,5,6]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) 
    | (x <= y) = x:(merge xs (y:ys))
    | otherwise = y:(merge (x:xs) ys)

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
nth [] n = Nothing
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
lookup' x [] = Nothing
lookup' x ((a,b):xs)
    | x == a  = Just b
    | otherwise = lookup x xs  

-- Exercise 6
-- Write a function that counts the number of times
-- an element appears in a list
count' :: (Eq a) => a -> [a] -> Int
count' e [] = 0
count' e (x:xs)
    | e == x = 1 + (count' e xs)
    | otherwise = count' e xs

-- Exercise 7
-- Write a function that takes a value e and a list of
-- values xs and remove all occurrences of e from xs

remove :: (Eq a) => a -> [a] -> [a]
remove e [] = []
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
alternate (x:y:xs) = y:alternate xs
alternate (x:xs) = xs

-- Exercise 9
-- Write a function
extract :: [Maybe a] -> [a]
-- that takes a list of Maybe values and return
-- the elements they contain. For example,
-- extract [Just 3, Nothing, Just 7]
extract [] = []
extract (Nothing:xs) = extract xs
extract ((Just a):xs) = a : extract xs


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
find str@(_:tail) substr
    | str `startsWith` substr = Just 0
    | otherwise = addMaybe $ find tail substr
    where addMaybe x = case x of Nothing -> Nothing
                                 Just v -> Just (v + 1)
          startsWith [] [] = True
          startsWith [] _ = False
          startsWith _ [] = True
          startsWith (s:ss) (x:xs) = and ((s == x):[(startsWith ss xs)])


-- test zone
main :: IO()
main = do print $ find "manoel" "noel"