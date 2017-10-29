{-- RECURSION --}

-- Recursion is a self referential style of definitions commonly used
-- in both mathematics and computer science.

-- Let's write some examples of recursive functions definitions
-- in Haskell

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)


-- A template for recursion over lists
{--
f :: [a] -> `type of result`
f [] = `result for empty list`
f (x:xs) = `result defined using (f xs) and x`
--}

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs


{-- Hand evaluation of length function


length [1,2,3]
= 1 + length [2,3]
= 1 + (1 + length [3])
= 1 + (1 + (1 + length []))
= 1 + (1 + (1 + 0))
= 0


--}


sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs


{-- Hand evaluation of sum function

sum [1,2,3]
= 1 + sum [2,3]
= 1 + (2 + sum[3])
= 1 + (2 + (3 + sum []))
= 1 + (2 + (3 + 0))
= 6


--}


append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : (append xs ys)


{-- Hand evaluation of function append

append [1,2,3] [4,5,6]
= 1 : (append [2,3] [4,5,6])
= 1 : 2 : (append [3] [4,5,6])
= 1 : 2 : 3 (append [] [4,5,6])
= 1 : 2 : 3 : [4,5,6]
= 1 : 2 : [3,4,5,6]
= 1 : [2,3,4,5,6]
= [1,2,3,4,5,6]
--}


zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip xs ys

{-- Hand evaluation of zip function

zip [1,2,3,4] ['a','b','c']
= (1,'a') : zip [2,3,4] ['b','c']
= (1,'a') : (2, 'b') : zip [3,4] ['c']
= (1, 'a') : (2, 'b') : (3, 'c') : zip [4]
= (1, 'a') : (2, 'b') : (3, 'c') = []
= [(1, 'a'), (2, 'b'), (3, 'c')]
--}


concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:xss) = append xs $ concat xss

{-- Hand evaluation of the concat function

concat' [[1], [2,3], [4,5,6]]
= append [1] $ concat [[2,3],[4,5,6]]
= append [1] $ append [2,3] $ concat [[4,5,6]]
= append [1] $ append [2,3] $ append [4,5,6] $ concat []
= append [1] $ append [2,3] $ append [4,5,6] []
= append [1] $ append [2,3] [4,5,6]
= append [1] [2,3,4,5,6]
= [1,2,3,4,5,6]
--}


quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (pivot:xs) =
  quicksort [y | y <- xs, y < pivot]
  ++ [pivot]
  ++ quicksort [y | y <- xs, y >= pivot]


{-- Hand evaluation of the quicksort

quicksort [3,8,5,2]
= quicksort [2] ++ [3] ++ quicksort [8,5]
= (quicksort [] ++ [2] ++ quicksort []) ++ [3] ++ quicksort [8,5]
= ([] ++ [2] ++ []) ++ [3] ++ (quicksort [5] ++ [8] ++ quicksort [])
= [2] ++ [3] ++ ((quicksort [] ++ [5] ++ quicksort []) ++ [8] ++ [])
= [2,3] ++ (([] ++ [5] ++ []) ++ [8])
= [2,3] ++ ([5] ++ [8])
= [2,3] ++ [5,8]
= [2,3,5,8]
--}


-- Higher Order Recursive Functions


map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map f xs


{-- Hand evaluation of map function

map (*5) [1,2,3]
= 1 * 5 : map (*5) [2,3]
= 5 : (2*5 : map (*5) [3])
= 5 : 10 : 3 * 5 : map (* 5) []
= 5 : 10 : 15 : []
= [5,10,15]

--}

foldr' :: (a -> a -> a) -> a -> [a] -> a
foldr' _ z [] = z
foldr' f z (x:xs) = f x $ foldr' f z xs

foldl' :: (a -> a -> a) -> a -> [a] -> a
foldl' _ z [] = z
foldl' f z (x:xs) = foldl' f (f z x) xs

sum'' :: (Num a) => [a] -> a
sum'' xs = foldr' (+) 0 xs


main = print $ sum'' [1,2,3]
