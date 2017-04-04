{-- EXERCISES --}

import Notes
  ( BinTreeInt(LeafInt, NodeInt)
  , BinTree(Leaf, Node)
  , inorder
  , height
  )

import TreePrinter

-- Exercise 1
-- Define a Haskell datatype Tree1 for a tree that contains a
-- character and an integer in each node, along with exactly three
-- subtrees.

data Tree1 = Leaf1 | Node1 Integer Char Tree1 Tree1 Tree1
             deriving Show


-- Exercise 2
-- Define a Haskell datatype Tree2 for a free that contains an integer
-- in each node, and that allows each node to have any number of subtrees

data Tree2 = Leaf2 | Node2 Integer [Tree2]
             deriving Show


-- Exercise 3
-- Calculate the inorder traversal of tree3
tree3 :: BinTreeInt
tree3 = NodeInt 4
                (NodeInt 2
                         (NodeInt 1 LeafInt LeafInt)
                         (NodeInt 3 LeafInt LeafInt))
                (NodeInt 7
                         (NodeInt 5
                               LeafInt
                               (NodeInt 6 LeafInt LeafInt))
                         (NodeInt 8 LeafInt LeafInt))

inorderBinTreeInt :: BinTreeInt -> [Int]
inorderBinTreeInt LeafInt = []
inorderBinTreeInt (NodeInt a l r ) =
  inorderBinTreeInt l ++ [a] ++ inorderBinTreeInt r

tree3inorder = inorderBinTreeInt tree3
-- [1,2,3,4,5,6,7,8]

-- Exercise 4.
-- Suppose that a tree has type BinTree a, and we have a function
-- f :: a -> b. Write a new traversal function:
inorderf :: (a->b) -> BinTree a -> [b]
-- that traverses the tree using inorder, but it applies f to the
-- data value in each node before placing the result in the list.
-- For example, inorder tree6 produces [1,2,3,4,5,6,7] but
-- inorderf (2*) tree6 produces [2,4,6,8,10,12,14]

inorderf _ Leaf = []
inorderf f (Node a l r) =
  inorderf f l ++ [f a] ++ inorderf f r

inorderf' :: (a -> b) -> BinTree a -> [b]
inorderf' f t = map f $ inorder t

-- Exercise 5
-- Define two trees of size seven, one with the largest possible
-- height and the other with the smallest possible height

-- height max => 7
sevenTreeHuge :: BinTree Int
sevenTreeHuge = Node 1
                     Leaf
                     (Node 2
                           Leaf
                           (Node 3
                                 Leaf
                                 (Node 4
                                       Leaf
                                       (Node 5
                                             Leaf
                                             (Node 6
                                                   Leaf
                                                   (Node 7 Leaf Leaf))))))

-- smallest height -> 3 =~ sqrt(7)
sevenTreeBalanced :: BinTree Int
sevenTreeBalanced = Node 1
                         (Node 2
                               (Node 4 Leaf Leaf )
                               (Node 6 Leaf Leaf))
                         (Node 3
                               (Node 5 Leaf Leaf)
                               (Node 7 Leaf Leaf))

-- Exercise 6
-- Suppose that the last equation of the function balanced
-- were changed to the following:
-- balanced (Node x t1 t2) = balanced t1 && balanced t2
-- Give an example showing that the modified function returns
-- True for an unbalanced tree

unbalancedTree = Node 1
                      Leaf
                      (Node 2 Leaf Leaf)

{-- Hand written evaluation

balanced unbalancedTree
balanced (Node 1 l r)
= balanced l && balanced r
= balanced Leaf && balanced (Node 2 Leaf Leaf)
= True && balanced Leaf && balanced Leaf
= True && True && True
= True

--}

-- Exercise 7
-- Suppose that the last equation of the function balanced
-- were changed to the following:
-- balanced (Node x t1 t2) = height t1 == height t2
-- Give an example showing that the modified function
-- returns True for an unbalanced Tree.

{-- EXAMPLE
      1
     / \
    2   3
   /     \
  4       5

--}

unbalancedTree' = Node 1
                      (Node 2
                            Leaf
                            (Node 4 Leaf Leaf))
                      (Node 3
                            Leaf
                            (Node 5 Leaf Leaf))
{-- Hand written evaluation

balanced unbalancedTree'
balanced (Node 1 l r)
= height l == height r
= 2 == 2
= True

--}

-- Exercise 8
-- Define a function mapTree that takes a function and applies
-- it to every node in the tree, returning a new tree of results.
-- The type should be:
mapTree :: (a -> b) -> BinTree a -> BinTree b
-- This function is analogous to map, which operators over lists.

mapTree f Leaf = Leaf
mapTree f (Node a l r) = Node (f a) (mapTree f l) (mapTree f r)

-- Exercise 9
-- Write concatTree, a function that takes a tree of lists
-- a function that takes a tree of lists and concatenates
-- the lists in order from left to right. For example,
-- concatTree (Node [2] (Node [3,4] Leaf Leaf)
--                      (Node [5] Leaf Leaf))
-- ==> [3,4,2,5]

concatTree :: BinTree [a] -> [a]
concatTree Leaf = []
concatTree (Node xs l r) = concatTree l ++ xs ++ concatTree r

-- Exercise 10
-- Write zipTree, a functino that takes two trees and pairs each
-- of the corresponding elements in a list. Your function should
-- return Nothing if the two trees do not have the same shape.
-- For example

sameShape :: BinTree a -> BinTree b -> Bool
sameShape Leaf Leaf = True
sameShape (Node _ l1 r1) (Node _ l2 r2) =
  sameHeight l1 l2 && sameHeight r1 r2 && sameShape l1 l2 && sameShape r1 r2
  where sameHeight l r = height l == height r

zipTree :: BinTree a -> BinTree b -> Maybe [(a,b)]
zipTree t1 t2
  | sameShape t1 t2 = Just $ zip (inorder t1) (inorder t2)
  | otherwise = Nothing

-- Exercise 11. Write zipWithTree, a function that is like zipWith
-- except that it takes trees instead of lists. The first argument is
-- a function of type (a->b->c) the second argument is a tree with elements
-- of type a and the third argument is a tree with elements of type b.
-- The function returns a list with type [c]

-- IN THIS VERSIONS IS USED THE MAYBE MONAD BECAUSE... YES!
-- Why not use monads after all?
-- Monads ARE MONADS (most precise definition known)

zipWithTree :: (a -> b -> c) -> BinTree a -> BinTree b -> Maybe [c]
zipWithTree f t1 t2 = let z = zipTree t1 t2
                       in  zipMaybe z
  where zipMaybe Nothing = Nothing
        zipMaybe (Just xs) = Just [f a b | (a,b) <- xs]

-- Exercise 12. Write appendTree, a function that takes a binary tree and a
-- list, and appends the contents of the tree (traversed from left to right) to
-- the front of the list. For example,
-- appendTree (BinNode 2 (BinNode 1 BinLeaf BinLeaf)
--                       (BinNode 3 BinLeaf BinLeaf))
--            [4,5]
-- evaluates to [1,2,3,4,5]. Try to find an efficient solution that minimises
-- recopying.

appendTree :: BinTree a -> [a] -> [a]
appendTree Leaf xs = xs
appendTree (Node x l r) xs = appendTree l (x : appendTree r xs)
-- this is the same definition as g from inorder' on Notes.hs
-- the partial function to write a inorder more efficiently
