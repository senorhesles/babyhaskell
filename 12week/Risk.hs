{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad
import Control.Monad.Random
import Data.Array(Array,accumArray,assocs)

sort :: [Int] -> [Int]
sort xs = concat [replicate k x | (x,k) <- assocs count]
        where count :: Array Int Int
              count = accumArray (+) 0 range (zip xs (repeat 1))
              range = (0, maximum xs)

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

--instance Random Battlefield where
--  random = 

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army } deriving Show

threeInts :: Rand StdGen (DieValue,DieValue,DieValue)
threeInts =
  die >>= \i1 ->
  die >>= \i2 ->
  die >>= \i3 ->
  return (i1,i2,i3)

twoInts :: Rand StdGen (DieValue,DieValue)
twoInts =
  die >>= \i1 ->
  die >>= \i2 ->
  return (i1,i2)

threeIntss :: Rand StdGen (DieValue,DieValue,DieValue)
threeIntss = die >>= (\i1 -> die >>= (\i2 -> die >>= (\i3 -> return (i1,i2,i3))))

die1 :: (RandomGen g) => Rand g Int
die1 = getRandomR (1,6)

die2 :: (RandomGen g) => Rand g Int
die2 = getRandomR (1,6)

dice :: (RandomGen g) => Int -> Rand g [Int]
dice n = sequence (replicate n die1)

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield 0 0) = do
  return (Battlefield 0 0)
battle (Battlefield 1 0) = do
  return (Battlefield 1 0)
battle (Battlefield 2 1) = do
  a1 <- rollDie
  b1 <- rollDie
  let bb = battlez (map unDV [a1]) (map unDV [b1]) (2,1)
  return (Battlefield (fst bb) (snd bb))
battle (Battlefield 3 1) = do
  a1 <- rollDie
  a2 <- rollDie
  b1 <- rollDie
  let bb = battlez (map unDV [a1,a2]) (map unDV [b1]) (3,1)
  return (Battlefield (fst bb) (snd bb))
battle (Battlefield 2 2) = do
  a1 <- rollDie
  b1 <- rollDie
  b2 <- rollDie
  let bb = battlez (map unDV [a1]) (map unDV [b1,b2]) (2,2)
  return (Battlefield (fst bb) (snd bb))
battle (Battlefield 3 2) = do
  a1 <- rollDie
  a2 <- rollDie
  b1 <- rollDie
  b2 <- rollDie
  let bb = battlez (map unDV [a1,a2]) (map unDV [b1,b2]) (3,2)
  return (Battlefield (fst bb) (snd bb))
battle (Battlefield x y) = do
  a1 <- rollDie
  a2 <- rollDie
  a3 <- rollDie
  b1 <- rollDie
  b2 <- rollDie
  let bb = battlez (map unDV [a1,a2,a3]) (map unDV [b1,b2]) (x,y)
  return (Battlefield (fst bb) (snd bb))

invade 

battlez :: [Int] -> [Int] -> (Int,Int) -> (Int,Int)
battlez [] _ (x,y) = (x,y)
battlez _ [] (x,y) = (x,y)
battlez xs ys (x,y) =
  let
    f = reverse $ sort xs
    s = reverse $ sort ys
    mf = maximum f
    ms = maximum s
    resultz = win mf ms
  in
    battlez (tail f) (tail s) (score resultz (x,y))

score :: Bool -> (Int,Int) -> (Int,Int)
score True (x,y) = (x,(y - 1))
score _ (x,y) = ((x - 1),y)

win :: Int -> Int -> Bool
win x y
  | x > y = True
  | otherwise = False

rollDie :: Rand StdGen DieValue
rollDie = do
  i <- getRandomR (1,6)
  return i

main = do
  values <- evalRandIO threeIntss
  morevals <- evalRandIO (dice 2)
  first <- evalRandIO die1
  secon <- evalRandIO die1
  moredies <- evalRandIO die
--  battles <- evalRandIO (battle testBattle)
  trip <- evalRandIO threeInts
  dubs <- evalRandIO twoInts
  rslts <- evalRandIO (battle (Battlefield 5 5))
  putStrLn (show rslts)
  
