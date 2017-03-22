{-- EXERCISES --}

-- Exercise 22
-- Assume there is a function called max that delivers the larger of
-- its two arguments
-- max x y = x  | if x >= y
-- max x y = y  | if y >= x
-- Write a function maximum that, given a non-empty sequence of values
-- whose sizes can be compared (that is values from a type of class Ord)
-- delivers the largest value in the sequence

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

-- Exercise 24
-- Write a function that, given a sequence containing only non-empty
-- sequences, delivers the sequence made up of the first elements of
-- each of those non-empty sequences.

concat' :: [[a]] -> [a]
concat' [[]] = []
concat' [xs] = xs
concat' (xs:xss) = xs ++ concat' xss


-- Exercise 26
-- Define an `and` operator using && and foldr

and' :: [Bool] -> Bool
and' xs = foldr (&&) True xs
