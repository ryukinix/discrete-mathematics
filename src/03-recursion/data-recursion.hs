{-- Data Recursion Topic --}

-- Peano Arithmetic

-- We turn now to the implementation of arithmetic operations over
-- a simple data structure representing the natural numbers. The reason
-- for working with this represantation is that it provides a good
-- introduction to recursion over general algebraic types

data Peano = Zero | Succ Peano
             deriving Show

peanoDecode :: Peano -> Integer
peanoDecode Zero = 0
peanoDecode (Succ x) = 1 + peanoDecode x

-- peanoDecode (Succ (Succ Zero)) == 2
-- peanoDecode Zero == 0

-- As you can see, the peano data type is recursive. In this case,
-- the recursion builds up a series of constructor applicationrs,
-- somewhat like the list data type

data List a = Empty | Cons a (List a)

listDecode :: List a -> [a]
listDecode Empty = []
listDecode (Cons a b) = a : listDecode b
-- listDecode (Cons 1 (Cons 2 (Cons 3 Empty))) == [1,2,3]
-- listDecode Empty == []

-- A few function to handle the Peano datatype
decrement :: Peano -> Peano
decrement Zero = Zero
decrement (Succ a) = a

add :: Peano -> Peano -> Peano
add Zero b = b
add (Succ a) b = Succ (add a b)

sub :: Peano -> Peano -> Peano
sub a Zero = a
sub Zero b = Zero
sub (Succ a) (Succ b) = sub a b


equals :: Peano -> Peano -> Bool
equals Zero     Zero     = True
equals Zero     b        = False
equals a        Zero     = False
equals (Succ a) (Succ b) = equals a b


lt :: Peano -> Peano -> Bool
lt a        Zero     = False
lt Zero     (Succ b) = True
lt (Succ a) (Succ b) = lt a b

-- Data Recursion

-- Another important programming technique uses recursion to define data
-- structures; this is called data recursion
-- The idea is to define circular data structures. Here is one way to define
-- an infinitely long list where every element is 1.

repeat' :: a -> [a]
repeat' x = x : repeat' x
ones = repeat' 1  -- infinite list of 1


-- However, it's possible to represent ones very compactly with a circular list
-- defined with recursion in the data rather than in a function

twos = 2 : twos -- infinite list of 2

-- A more tricky example
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
-- Yes! The fibonnaci sequence! Infinite! One-liners is the power of Haskell



main :: IO ()
main = print $ take 20 twos