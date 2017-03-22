-- This notes comes from learnyouahaskell.com/modules

-- exists a lot of ways to import external modules on haskell
import Data.List -- put all Data.List definitions on the global namespace
import Data.List (sort, nub) -- only import sort and nub
import Data.List hiding (sort) -- import all from Data.List, except for sort
import qualified Data.Map as Map  -- import this module as reserved namespace M
import Data.Char
import Data.Function (on)
import qualified Data.Set as Set

-- brief examples of combined functions from Data.List, Data.Char and Data.Function
pythonSplit a s = filter (not . any isChar) . groupBy ((==) `on` isChar) $ s
  where isChar = (== a)

commaInts :: String -> [Int]
commaInts s = map read $ pythonSplit ',' s

pythonJoin = intercalate

encode :: Int -> String -> String
encode shift msg =
  let ords = map ord msg
      shifted = map (+ shift) ords
   in map chr shifted

decode :: Int -> String -> String
decode shift = encode (negate shift)

-- The main proposal of Data.Map


-- a naive implementation of findKey for association lists
findKey :: (Eq k) => k -> [(k,v)] -> v
findKey k xs = snd . head . filter (\(a,b) -> k == a) $ xs


-- a more secure using the maybe monad
findKey' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey' _ [] = Nothing
findKey' key ((k,v):xs) = if key == k
                             then Just v
                             else findKey' key xs

-- association lists are a primitive way to dealing with
-- key-value store. Instead that we can use the Map.Map
-- which are more efficient because the data are stored
-- in a tree


-- Map.Map
-- Map.empty
-- Map.insert
-- Map.null
-- Map.lookup
-- Map.singleton
-- Map.map
-- Map.filter
-- Map.member
-- Map.keys
-- Map.elems
-- Map.fromList
-- Map.toList
-- Map.fromListWith
-- Map.insertWith

-- This above are the mainly used functions from the
-- Data.Map module.
