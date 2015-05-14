
import Prelude

data Tree a = Leaf | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)

foldTree :: (Eq a) => [a] -> Tree a
foldTree = foldr inTree Leaf

treeToDep :: Tree a -> Integer
treeToDep (Leaf) = (-1)
treeToDep (Node x Leaf _ Leaf) = 0
treeToDep (Node x y _ z) = (max (treeToDep y) (treeToDep z)) + 1

inTree :: (Eq a) => a -> Tree a -> Tree a
inTree a (Leaf) = (Node 0 Leaf a Leaf)
inTree a (Node x y z u)
  | (y == Leaf) && (u == Leaf) = (Node ((treeToDep (inTree a y)) + 1) (inTree a y) z u)
  | ((treeToDep y) == (treeToDep u)) && ((treeToDep (inTree a y)) == (treeToDep y)) = (Node ((treeToDep (inTree a y)) + 1) (inTree a y) z u)
  | ((treeToDep y) < (treeToDep u)) = (Node ((treeToDep (inTree a y)) + 1) (inTree a y) z u)
  | otherwise = (Node ((treeToDep (inTree a u)) + 1) y z (inTree a u))

{-

The way the preceding program works is like this:

The function *foldTree* takes a list of stuff, and attempts to create a balanced Binary Tree out of it. The way it does this is by folding the function *inTree* with a list. It starts out assuming that the first item on your list, will be folded into a Leaf.

If we go down to *inTree* what we find is that when *a* is applied to *Leaf*, it results in a Node, of depth zero, and with two Leafs as daughters.

Subsequently, we come to the real way that the function reasons about stuff.

The first guard states that, if you're trying to apply *a* to a *Node z*, and both of *z's* daughters are *Leafs*, then, you apply *a* to the daughter on the left, take the Depth of the daughter on the left, add one to it, and make it the new depth of *z*.

This is what we want. Assuming you have tree with only leaves as daughters, its depth is going to be zero, and so, if we apply *a* to it, its going to create a new Node into its left daughter, and so the depth of *z* would be 1

What guard two attempts to do is, first it checks if the depth of z's daughter's are equal, and if they are, it checks if, by applying *a* to the daughter on the left, the daughter on the left's depth won't change. If these two conditions check out, it applies *a* to the daughter on the left. Why did I do this? The first condition seems straightforward, but the second condition seems strange. The rationale for the second condition is that, it could be the case that Left daughter consisted of two Nodes, and so, left Daughter's depth would be at least 1, but Right daughter could only have one Node, and a leaf, but right Daughter's depth would be at least 1 as well. So, without the condition, the function would apply *a* to Left daughter...which doesn't immediately result in an unbalanced tree, but left daughter would have a depth of at least 2, and Right daughter would still only have a depth of at least 1...and it would still have a leaf! No good.

So, we make sure that that doesn't happen by making sure that if Left daughters depth would increase when its sister had the same depth, it instead applies it to right daughter.

The third guard acts as a balance to the second guard. It simply states that when left daughter's depth is less than right daughter's depth, apply *a* to left daughter

The last guard, which applies *a* to right daughter, occurs only when applying *a* to left daughter would increase its depth when the depth of the two daughters was equal, filling in the holes on the right side. This ensures that that any two branches remain balanced.

-}
