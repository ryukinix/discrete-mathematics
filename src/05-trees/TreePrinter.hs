import Data.Function (on)
import Data.List (sortBy, groupBy, intercalate)
import Notes (BinTree(Node, Leaf))


walkTree :: BinTree a -> [(Int, a)]
walkTree Leaf = []
walkTree root@(Node a l r) = let h = height root
                                 left = walkTree l
                                 right = walkTree r
                             in [(h,a)] ++ left ++ right

treeList :: BinTree b -> [(Int,[b])]
treeList t = clean $ groups $ walkTree t
  where groups = groupBy ((==) `on` fst ) . sortBy (compare `on` fst)
        f xs = (fst (head xs), map snd xs)
        clean = map f

joinLine :: (Show a) => [a] -> String
joinLine xs = intercalate " " $ map show xs

muchPrettyTree :: Show a => BinTree a -> String
muchPrettyTree t = unlines .  map parserLine $ treeList t
  where parserLine (d,xs) = replicate (d+2) ' ' ++ joinLine xs

prettyTree :: (Show a) => BinTree a -> Int -> String
prettyTree Leaf _ = ""
prettyTree (Node a left right) d = (replicate d ' ') ++ "=> " ++ show a ++ "\n"
                                   ++ lookBranch left ++ lookBranch right
  where lookBranch b = prettyTree b (d + 3)

height :: BinTree a -> Int
height Leaf = 0
height (Node _ l r) = 1 + max (height l) (height r)

instance (Show a) => Show (BinTree a) where
  show t = prettyTree t 0

testTree = Node 10 (Node 12 (Node 13 Leaf Leaf) (Node 14 Leaf Leaf)) (Node 11 Leaf Leaf)

main :: IO()
main = print $ testTree
