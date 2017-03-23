{-- TREES --}
module Notes
  ( BinTree(Leaf, Node)
  ) where

-- Binary Tree of a Arbitrary Type


data BinTree a
  = Leaf
  | Node a (BinTree a) (BinTree a)
