{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Employee
import OrphanGuestList
import Data.Tree

testCompany3 :: Tree Integer
testCompany3
  = Node (9)
    [ Node (2)
      [ Node (5)
        [ Node (1) []
        , Node (5) []
        ]
      , Node (3) []
      ]
    , Node (17)
      [ Node (4) []
      ]
    ]


glCons :: Employee -> GuestList -> GuestList
glCons (Emp x y) (GL z u) = (GL (z ++ [Emp x y]) (y + u))

moreFun :: GuestList -> GuestList -> GuestList
moreFun (GL x y) (GL z u)
  | y > u = (GL x y)
  | otherwise = (GL z u)

--treeFold :: (a -> a -> a) -> a -> [Tree a] -> Tree a -> a
treeFold f e [] (Node x []) = f x e
treeFold f e [] (Node x y) = f x (treeFold f e (tail y) (head y))
treeFold f e z (Node x y) = f x (treeFold f e ((tail z) ++ y) (head z))

treeFold3 f e zs (Node x (y:ys)) = f (treeFold3 f e (zs ++ ys) y) x
treeFold3 f e [] (Node x []) = f e x

tF2 e (z:zs) f (Node x (y:ys)) = f (tF2 e ys f y) x (tF2 e zs f z)
tF2 e (z:zs) f (Node x []) = f (tF2 e [] f (Node x [])) x (tF2 e zs f z)
tF2 e [] f (Node x (y:ys)) = f (tF2 e ys f y) x (tF2 e [] f (Node x []))
tF2 e [] f (Node x []) = f e x e

tF3 :: b -> [Tree a] -> (b -> a -> b) -> Tree a -> b
tF3 e zs f (Node x (y:ys)) = f (tF3 e (ys ++ zs) f y) x 
tF3 e (z:zs) f (Node x []) = f (tF3 e zs f z) x
tF3 e [] f (Node x []) = f e x

{-
Ok, I finally figured out what it means when a type is a -> b rather than a -> a

What it means when its "a -> a" is, obviously, the input type and the output type
have to be the same. End of story. Don't think about going from strings to ints
or anything like that.

When it goes from a -> b, because we're defining a polymorphic function, when
we implement our function, we have to be VERY careful that the implementation
doesn't use any information particular to the input type. I'll give you an example.

Refer to treeFold and tF3. treeFold has type:
"(a -> a -> a) -> a -> [Tree a] -> Tree a -> a". tF3 has type:
"b -> [Tree a] -> (b -> a -> b) -> Tree a -> b". What does this mean? Why did they
end up this way? Why are they different?

Go to treeFold. Look at how the function 'f' behaves. It takes two inputs, and it gives an output. But look at where the two inputs come from. Look at the second and third line. The inputs are (head y) (tail y) in the first one, and ((tail z) ++ y) and (head z). The second line is the culprit. 'y' has the type of whatever type the Node has...so, the compiler has to interpret this to mean that it has whatever type the Node has...but why does it assume that it must be the same type as our initial dummy variable 'e'? Why couldn't 'e' be different?

Well. Nevermind.
-}
