{-- HIGHER ORDER FUNCTIONS --}

-- The function twice receive a function and
-- its argument and apply two times on it.
twice :: (a->a) -> a -> a
twice f x = f (f x)


prod :: Integer -> Integer -> Integer
prod x y = x * y
g = prod 4 -- partial application! this is a function
p = g 6 -- 24
q = twice g 3 -- => 48

-- foldr, foldl as reduce/accumulator
fat :: Integer -> Integer
fat n = foldr (*) 1 [1..n]

-- mapping values using map
s = map (\n -> n + 1) [1..10]
-- => sum +1 for each value of [1..10]
-- => [2,3,4,5,6,7,8,9,10,11]


-- zipWith definition
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- fliping function arguments
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
  where g x y = f y x


-- filtering
evens1to10 = filter even [1..10]

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000, 99999..])
  where p x = x `mod` 3829 == 0

-- takeWhile get the the values until condition matchs
n = sum $ takeWhile (<10000) $ filter odd $ map (^2) $ [1..]
v = takeWhile (/=' ') "bananas assadas" -- bananas
