{-- Equational Reasoning using Haskell --}

{--
One of the formal methods to proof is the equational reasoning.
Nothing more than the school algebra with a few notations more.

x = 8
x = 4

Suppose we wants evaluate the following expression:


2*x + x/y
  = 2 * 8 + 8/y     { x }
  = 2 * 8 + 8/4     { y }
  = 16 + 2          { arithmetic }
  = 18              { arithmetic}

2 * x + x/y = 18


The example above is just elementary mathematics, but exactly
the same technique can be used to reason about Haskell programs.
Because equations in Haskell are true mathematical equations
-- they are not assignment statements.

--}

-- Equational Reasoning as Hand-execution
-- We can use Equation Reasoning to proof the correctness of an algorithm


f :: Integer -> Integer -> Integer
f x y = (2+x) * g y

g :: Integer -> Integer
g z = 8 - z

-- Hand execution
{--
f 3 4
  = (2 + 3) * g 4     { f }
  = (2 + 3) (8 - 4)   { g }
  = 20                {arithmetic}
--}

-- Conditionals

{--
  A conditional expression satisfies the following equations

if True then e2 else e3 = e2    {if True}
if False then e2 else e3 = e3   {if False}
--}


f' :: Double -> Double
f' x =
  if x >= 0
     then sqrt x
     else 0

-- f' 4 => 2.0
-- f' -1 => 0

-- EQUATIONAL REASONING WITH LISTS


-- Theorem 1 (length (++))
-- Let xs, ys :: [a] be arbitrary lists. Then:
-- length (xs ++ ys) = length xs + length ys

-- Theorem 2 (length map)
-- Let xs :: [a] be an arbitrary list, and f :: a -> b an arbitrary function
-- Then length (map f xs) = length xs

-- Theorem 3 (map (++))
-- Let xs, ys :: [a] be arbitrary lists, and let f :: a -> b be an arbitrary function.
-- Then map f (xs ++ ys) = map f xs ++ map f ys

-- Theorem 4
-- For arbitrary lists xs, ys :: [a] and arbitrary f :: a -> b, the
-- following equations holds:
-- length (map f (xs ++ ys)) = length xs + length ys


-- Proof Theorem 4 using as Lema Theorem 1,2,3
{--
length (map f (xs ++ ys))
   = length (map f xs ++ map f ys)          { map (++) }    (Theorem 3)
   = length (map f xs) + length (map f ys)  { length (++) } (Theorem 1)
   = length xs + length ys                  { length map }  (Theorem 2)

--}


-- NOTE: A variable in mathematics means "a name that stands for some value"
--       while a variable in a imperative programming language means
--       "a memory location whose value is modified by certain instructions"
