
import Prelude

import Data.List

getIntegerPairs :: Integer -> [(Integer, Integer)]
getIntegerPairs n = cartProd [1..n] [1..n]

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

combineIntList :: [Integer] -> [Integer] -> [Integer]
combineIntList (x:xs) (y:ys) = [((x + y) + (2 * x * y))] ++ (combineIntList xs ys)
combineIntList [] [] = []

combineUpTo :: Integer -> [Integer]
combineUpTo n =
  let
    first = map fst $ getIntegerPairs n
    secon = map snd $ getIntegerPairs n
  in
   combineIntList first secon

sieve :: Integer -> [Integer]
sieve n =
  let
    composites = combineUpTo n
    notComposites = ([1..n] \\ composites)
    almostPrimes = map (2*) notComposites
    primes = map (1+) almostPrimes
  in
   primes



