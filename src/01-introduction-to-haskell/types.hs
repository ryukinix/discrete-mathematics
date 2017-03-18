{--
   Just a simple and common types on Haskell

Int stands for integer with fixed precision
Integer stands for a integer with arbitrary precision (BigNum)
Float is real floating point with single precision
Double is a real floating point with double the precision
Bool is a boolean type. It can have only two values: True and False.
Char represents a character. It's denoted by single quotes.
String is a list of characters.
IO handles Input and Output

--}



-- Number types
int :: Int
integer :: Integer
float :: Float
rational :: Rational

-- Logic type
bool :: Bool

-- IO () (Input/Output)
main :: IO ()

int = 1
integer = 2 ^ 100
float = 3/4
rational = 3/4
bool = True

main = do print int
          print integer
          print float
          print rational
          print bool
