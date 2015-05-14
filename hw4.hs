
import Prelude

data Tree a = Leaf | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)

--foldTree :: [a] -> Tree a
--foldTree = foldr

foldTreeInterMediary :: a -> Tree a
foldTreeInterMediary a = (Node 0 Leaf a Leaf)

insertEquivalent :: a -> Tree a -> Tree a
insertEquivalent a (Node x y z u)
  | ((y == Leaf) && (u == Leaf)) = (Node (x + 1) y z (Node 0 Leaf a Leaf))
  | ((y /= Leaf) && (u == Leaf)) = (Node x y z (Node 0 Leaf a Leaf))
  | ((y = Leaf) && (u /= Leaf)) = (Node x (Node 0 Leaf a Leaf) z u)
  | ((y /= Leaf) && (u /= Leaf)) && ((tree2Depth y) > (tree2Depth u)) = insertEquivalent a u
  | ((y /= Leaf) && (u /= Leaf)) && ((tree2Depth y) < (tree2Depth u)) = insertEquivalent a y

tree2Depth :: Tree a -> Integer
tree2Depth (Node x _ _ _) = x

