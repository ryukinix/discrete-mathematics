{--
    A few examples of basic structures on Haskell

    Tuples, Lists and Strings

--}

import Data.Char (toUpper, toLower) -- importing toUpper and toLower
import Text.Printf


-- STRINGS AND CHARS
{--
Strings are made by elements chars.
A elemental string can be see as a list of chars.

> ['a', 'b', 'c', 'd'] == "abcd"
=> True

Single quotes denotes characters.
Double quotes denotes strings.
--}


upperChar :: Char
lowerChar :: Char
upperString :: String

upperChar = toUpper 'w' -- => 'W'
lowerChar = toLower upperChar -- => 'w'
upperString = map toUpper "asdf" -- => "ASDF"

{-- TUPLES AND LISTS

tuple_example = ('a', 'b', 'c')
=> A tuple of length three containing the characters a, b and c.
list_example = ['a', 'b', 'c']
=> A list of length three containing the characters a, b and c.

Types =>
tuple_example :: (Char, Char, Char)
list_example :: [Char]

:: Tuples

   Tuples have two important characteristics. First, a tuple has
a fixed number of components. If you have a 3-tuple (a, b, c),
and you add an extra data value to obtain (a, b, c, d) the new tuple
hash a different type from the old one. Second, there is no restriction
on the type of any component: it is common for a tuple to contain data values
with different types.


:: Lists

   A list may contain any number of elements, but all of the elements must
have the same type (homogeneous lists). The type of a list is written [A],
where A is the element type.

--}

tupleOfStrings :: (String, String)
tupleOfStrings = ("dog", "cat")
dog = fst tupleOfStrings
cat = snd tupleOfStrings

nums :: [Int]
catDog :: [String]
nestedList :: [[Int]]
nums = [13, 9, -2, 100]
catDog = ["cat", "dog"]
nestedList = [[1,2],[3,7,1], [], [900]]

-- You can specify ranges using lists
range1to10 = [1..10] :: [Int]
alphabet = ['a'..'z'] :: [Char]

-- As well we can define sequences
sequence' = [10,8..0] :: [Int]
-- => [10,8,6,4,2,0]


-- List Notation and (:)
listCons = 1:2:3:[]
listSyntaxSugar = [1,2,3]
isEqual = listCons == listSyntaxSugar
