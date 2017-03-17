{--
   Just a simple and common types on Haskell
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
integer = 2^200
float = 3/4
rational = 3/4
bool = True

main = do print int
          print integer
          print float
          print rational
          print bool
