{-- TREES --}
module Notes
  ( BinTree(Leaf, Node)
  , BinTreeInt(LeafInt, NodeInt)
  , inorder
  , posorder
  , preorder
  , reflect
  , size
  , height
  , binTreeIntToBinTree
  , balanced
  , linSearch
  , binSearch
  , insert
  ) where

-- Binary Tree of Int nodes

data BinTreeInt
  = LeafInt
  | NodeInt Int BinTreeInt BinTreeInt


-- Binary Tree of a Arbitrary Type


data BinTree a
  = Leaf
  | Node a (BinTree a) (BinTree a)


-- Traversal modes

preorder :: BinTree a -> [a]
preorder Leaf = []
preorder (Node a l r) = [a] ++ preorder l ++ preorder r

inorder :: BinTree a -> [a]
inorder Leaf = []
inorder (Node a l r) = inorder l ++ [a] ++ inorder r

posorder :: BinTree a -> [a]
posorder Leaf = []
posorder (Node a l r) = posorder l ++ posorder r ++ [a]


-- Processing Tree Structure

-- given a BinTree<a> returns a reflected BinTree<a>
-- (swaps the branchs of each node)
reflect :: BinTree a -> BinTree a
reflect Leaf = Leaf
reflect (Node a l r) = Node a (reflect r) (reflect l)

-- given a BinTree<a> returns a Int number called height
-- based on the current node and the deepest node on the tree
height :: BinTree a -> Integer
height Leaf = 0
height (Node a l r) = 1 + max (height l) (height r)


-- given a BinTree<a> returns a Int number called size
-- based on the count of nodes on the tree
size :: BinTree a -> Integer
size Leaf = 0
size (Node a l r) = 1 + size l + size r

-- given a BinTree<a> returns True if the tree is fully
-- balanced and Fals otherwise
balanced :: BinTree a -> Bool
balanced Leaf = True
balanced (Node _ l r) =
  height l == height r && balanced l && balanced r


-- :: Search Algorithms

-- Linear Search
linSearch :: (Eq a) => a -> [(a,b)] -> Maybe b
linSearch _ [] = Nothing
linSearch k ((a,b):xs) =
  if a == k
     then Just b
     else linSearch k xs

-- Binary Search
binSearch :: (Ord a) => a -> BinTree (a,b) -> Maybe b
binSearch _ Leaf = Nothing
binSearch k (Node (a,b) l r)
  | k == a = Just b
  | k > a  = binSearch k r
  | k < a  = binSearch k l

-- Insert (for Binary Tree)

insert :: Ord a => (a,b) -> BinTree(a,b) -> BinTree(a,b)
insert pair Leaf = Node pair Leaf Leaf
insert newPair@(key,v) (Node oldPair@(a,_) l r)
  | key == a = Node (key,v) l r
  | key > a = Node oldPair l (insert newPair r)
  | key < a = Node oldPair (insert newPair l) r


-- auxiliary functions
binTreeIntToBinTree :: BinTreeInt -> BinTree Int
binTreeIntToBinTree LeafInt = Leaf
binTreeIntToBinTree (NodeInt a l r) =
  Node a (binTreeIntToBinTree l) (binTreeIntToBinTree r)
