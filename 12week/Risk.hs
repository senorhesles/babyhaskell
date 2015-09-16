{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random

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
battle (Battlefield x y) = do
  a1 <- getRandomR (1,6)
  a2 <- getRandomR (1,6)
  a3 <- getRandomR (1,6)
  b1 <- getRandomR (1,6)
  b2 <- getRandomR (1,6)
  | (x == 1) = return (Battlefield x y)
  | (x == 2) &&


testBattle :: Battlefield
testBattle = Battlefield 15 14

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
  putStrLn (show first)
  
