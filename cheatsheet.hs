
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


n :: Integer
n = 1234567890987654321987340982334987349872349874534

reallyBig :: Integer
reallyBig = 2^(2^(2^(2^2)))

numDigits :: Int
numDigits = length (show reallyBig)

d1, d2 :: Double
d1 = 4.5387
d2 = 6.2831e-4

ex01 = 3 + 2
ex02 = 19 - 27
ex03 = 2.35 * 8.6
ex04 = 8.7 / 3.1
ex05 = mod 19 3
ex06 = 19 `mod` 3
ex07 = 7 ^ 222
ex08 = (-3) * (-7)
i1 :: Integer
i1 = i1 `div` i1
-- Integrals: Integer and Int
-- Floats: Double and Float
ex11 = True && False
ex12 = not (False || True)
ex13 = ('a' == 'a')
ex14 = (16 /= 3)
ex15 = (5 > 3) && ('p' <= 'q')
ex16 = "Haskell" > "C++"

-- Compute the sum of the integers from 1 to n.
sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial whatev = whatev + sumtorial (whatev-1)

mariotorial :: String -> String
mariotorial "mario" = "he's a dingus" -- these are examples of cases
mariotorial _ = "who?" -- wildcard case!! watch out

hailstone :: Integer -> Integer
hailstone n
  | n `mod` 2 == 0 = n `div` 2 -- Must be a boolean when using guards
  | otherwise = 3*n + 1

foo :: Integer -> Integer
foo 0 = 16
foo 1 -- This is a clause, and it's pattern '1'...if its pattern is matched
  | "Haskell" > "C++" = 3 -- this is a guard...guards are chosen from top to bottom...first which evaluates to true is choosen
  | otherwise         = 4
foo n -- this is the second clause...we only get here if none of the preceding guards is eval tru
  | n < 0            = 0
  | n `mod` 17 == 2  = -43
  | otherwise        = n + 3


isEven n = n `mod` 2 == 0


-- PAIRS
p :: (Int, Char)
p = (3, 'x')

sumPair :: (Double,Double) -> Double
sumPair (x,y) = x + y

f :: Int -> Int -> Int -> Int
f x y z = x + y + z

-- Function application has priority over infix operators

nums, range, range2 :: [Integer]
nums   = [1,2,3,19]
range  = [1..100]
range2 = [2,4..100]

person :: String
person = "Mario"

hello1 :: [Char]
hello1 = ['h', 'e', 'l', 'l', 'o']

hello2 :: String
hello2 = "hello"

emptyList = []

ex18 = 1 : []
ex19 = 3 : (1 : [])
ex20 = 2 : 3 : 4 : []
ex21 = [2,3,4] == 2 : 3 : 4 : [] -- these are singly linked lists NOT arrays

hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)

-- Compute the length of a list of Integers.
intListLength :: [Integer] -> Integer
intListLength []     = 0
intListLength (x:xs) = 1 + intListLength xs -- since we don't use x...can use _

sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo []         = []     -- Do nothing to the empty list
sumEveryTwo (x:[])     = [x]    -- Do nothing to lists with a single element
sumEveryTwo (x:(y:zs)) = (x + y) : sumEveryTwo zs

sumEveryThree :: [Integer] -> [Integer]
sumEveryThree [] = []
sumEveryThree (x:[]) = [x]
sumEveryThree (w:(x:(y:zs))) = (x + y + w) : sumEveryThree zs

-- The number of hailstone steps needed to reach 1 from a starting
-- number.
hailstoneLen :: Integer -> Integer
hailstoneLen n = intListLength (hailstoneSeq n) - 1


