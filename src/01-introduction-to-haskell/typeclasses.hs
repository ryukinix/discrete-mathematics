{-- Basic Haskell Typeclasses

Type classes are similar to interfaces on another languages.

A Type Class stands for its members have common properties in
a group of types.

On type declaration, the first part of an implication (=>)
are the type class constraints using Typeclasses. This ensures
that the determined type variables are members of the given typeclass.

* Eq is used for types that support equality testing.
* Ord is for types that have an ordering
* Show has the property of your members can be presented as strings.
* Read is sort of the opposite typeclass of Show.
* Enum members are sequentially ordered types -- they can be enumerated.
* Bounded members have an upper and a lower bound
* Num is a numeric typeclass.
* Integral is a subset of Num including the whole numbers: Int and Integer
* Floating is a subset of Num including only real nums: Float and Double.
--}


fat1 :: (Integral a) => a -> a
fat1 n = product [1..n]


lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs


capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]
