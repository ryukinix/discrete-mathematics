{-- List Comprehension

List comprehension is Haskell is very similar to set comprehension
in Mathematics. A list comprehension has this form:

[expression | generator]

The generator specifies a sequence of values that a variable takes on.
This is written in the form var <- list, and it means that the variable var
will take on each of the values in list, one by one. For each of those values,
the expression to the left of the bar is evaluated.

--}

import Data.Char (toLower)

multiplesOfTen :: [Int]
squares :: [Int]
lowering :: String

multiplesOfTen = [10 * x | x <- [1, 2, 3]]
-- => [10, 20, 30]
squares = [x ^ 2 | x <- [1..5]]
lowering = [toLower c | c <- "Too Many CAPITALs"]

-- destructuring bind
ab = [a * b | (a,b) <- [(1,2),(10,20), (6,6)]]
-- => [2,200,36]

-- multiples generators acting combinators or nested loops
xy :: [(Int, Char)]
xy = [(x, y) | x <- [1,2,3], y <- ['a', 'b']]
-- => [(1,'a'),(1,'b'),(2,'a'),(2,'b'),(3,'a'),(3,'b')]

-- specifying conditions to filter
multiplesOfTwoAndSeven = [x | x <- [0..100],
                              x `mod` 2 == 0 && x `mod` 7 == 0]
-- => [0,14,28,42,56,70,84,98]

-- Creating a list of factors of 12
factorsOfTwelve = [x | x <- [1..12],
                       y <- [1..12],
                       12 == x*y]

-- Exercise 2.

emptyList = [x | x <- [1, 2, 3], False]
-- => []

nandTruthTable = [not (x && y) | x <- [False, True],
                                   y <- [False, True]]
-- => [True, True, True, False]

justTrue = [x || y | x <- [False, True],
                     y <- [False, True],
                     x /= y]
-- => [True, True]

sphereCoordinates = [[x,y,z] | x <- [1..50],
                               y <- [1..50],
                               z <- [1..50],
                               x ^ 2 + y ^ 2 == z ^ 2]
-- => [[3,4,5],[4,3,5],[5,12,13],[6,8,10],[7,24,25],[8,6,10],[8,15,17],...]
