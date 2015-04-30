
import Data.List

doubleMe :: Int -> Int
doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

newSucc :: Int -> Int
newSucc x = x + 1

{- Haskell syntax -}
{-
True && False
2 + 15
5 / 2
True && True
False || True
not False
not (True && False)
5 == 5
1 /= 2
"hello"
[1,2,2,3,4]
[(1,2),(1,2)]
succ
min
max
div 92 10
-}

{- if then -}
doubleSmallNumber :: Int -> Int
doubleSmallNumber x = if x > 100
                      then x
                      else x*2

doubleSmallNumber' :: Int -> Int
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1


