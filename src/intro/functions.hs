{--

    This file explain the basic function to do:

    * type declaration
    * pattern matching
    * function definition
    * function composition

--}

-- :: factorial function
fat :: Integer -> Integer
fat 0 = 1
fat n | n > 0 = n * fat (n - 1)

-- :: fibonacci function

-- | domain and contra-domain declaration
fib :: Integer -> Integer
-- | pattern matching
fib n | n <= 1 = 1
-- | formal definition of fib function
fib n | n > 1 = fib(n - 1) + fib(n - 2)

-- | function composition
-- is just like fatfib = fat(fib(x))
fatfib = fat . fib

