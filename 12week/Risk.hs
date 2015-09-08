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

threeInts :: Rand StdGen (Int,Int,Int)
threeInts =
  getRandom >>= \i1 ->
  getRandom >>= \i2 ->
  getRandom >>= \i3 ->
  return (i1,i2,i3)

die1 :: (RandomGen g) => Rand g Int
die1 = getRandomR (1,6)

dice :: (RandomGen g) => Int -> Rand g [Int]
dice n = sequence (replicate n die1)

battle :: Battlefield -> Rand StdGen Battlefield
battle bf = return bf

testBattle :: Battlefield
testBattle = Battlefield 15 14

main = do
  values <- evalRandIO threeInts
  morevals <- evalRandIO (dice 2)
  first <- evalRandIO die1
  secon <- evalRandIO die1
  moredies <- evalRandIO die
  battles <- evalRandIO (battle testBattle)
  putStrLn (show values)
  putStrLn (show morevals)
  putStrLn (show moredies)
  putStrLn "hello"
  putStrLn (show battles)
  putStrLn (show (max first secon))
