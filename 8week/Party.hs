{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Employee
import OrphanGuestList
import Data.Tree

glCons :: Employee -> GuestList -> GuestList
glCons (Emp x y) (GL z u) = (GL (z ++ [Emp x y]) (y + u))

moreFun :: GuestList -> GuestList -> GuestList
moreFun (GL x y) (GL z u)
  | y > u = (GL x y)
  | otherwise = (GL z u)

--treeFold :: x -> Tree a -> b
--treeFold _ e (Node { subForest = [] }) = e
--treeFold f e x = f (rootLabel x) (treeFold f e (foldr treeFold (subForest x)))

--treeFold2 :: (a -> b -> b) -> b -> [Tree a] -> Tree a -> b
treeFold2 _ e _ (Node { subForest = [] }) = e
treeFold2 f e [] (Node x (y:ys)) = f x (treeFold2 f e ys y)

--data Tree a  = Node { rootLabel :: a, subForest :: Forest a } where Forest = [Tree a]

treeFun :: Tree Employee -> Fun
treeFun = treeFold2 (\x y -> x + (empFun (rootLabel y))) 0

--combineGLs :: Employee -> [GuestList] -> GuestList
--combineGLs 
