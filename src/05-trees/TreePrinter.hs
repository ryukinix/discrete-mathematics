module TreePrinter
  (prettyTree) where

import Notes (BinTree(Node, Leaf))

prettyTree :: (Show a) => BinTree a -> Int -> String
prettyTree Leaf _ = ""
prettyTree (Node a left right) d = (replicate d ' ') ++ "=> " ++ show a ++ "\n"
                                   ++ lookBranch left ++ lookBranch right
  where lookBranch b = prettyTree b (d + 3)

instance (Show a) => Show (BinTree a) where
  show t = prettyTree t 0
