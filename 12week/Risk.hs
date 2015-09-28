{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad
import Control.Applicative
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

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army } deriving Show

attToDice (Battlefield x y)
  | x >= 4 = threeInt
  | x == 3 = twoInt
  | x == 2 = oneInt


defToDice (Battlefield x y)
  | y >= 3 = threeInt
  | y == 2 = twoInt
  | y == 1 = oneInt

threeInt =
  die >>= \i1 ->
  die >>= \i2 ->
  die >>= \i3 ->
  return [i1,i2,i3]

twoInt =
  die >>= \i1 ->
  die >>= \i2 ->
  return [i1,i2]

oneInt =
  die >>= \i1 ->
  return [i1]

strongToWeak xs = reverse $ sort $ map unDV xs

battleInd :: (Int,Int) -> Battlefield -> Battlefield
battleInd (x,y) (Battlefield att def)
  | x > y = (Battlefield att (def - 1))
  | x < 2 = (Battlefield att def)
  | y == 0 = (Battlefield att def)
  | otherwise = (Battlefield (att - 1) def)

battleList :: [(Int,Int)] -> Battlefield -> Battlefield
battleList list bf = foldr battleInd bf list

battleFinal :: Battlefield -> Rand StdGen Battlefield
battleFinal (Battlefield 0 y) = return (Battlefield 0 y)
battleFinal (Battlefield x 0) = return (Battlefield x 0)
battleFinal (Battlefield 1 y) = return (Battlefield 1 y)
battleFinal bf = do
  att <- attToDice bf
  def <- defToDice bf
  let
    attSort = strongToWeak att
    defSort = strongToWeak def
    engage = zip attSort defSort
    battleF = battleList engage bf
  return battleF

invadeFinal :: Battlefield -> Rand StdGen Battlefield
invadeFinal (Battlefield 0 y) = return (Battlefield 0 y)
invadeFinal (Battlefield x 0) = return (Battlefield x 0)
invadeFinal (Battlefield 1 y) = return (Battlefield 1 y)
invadeFinal bf = do
  v <- battleFinal bf
  vs <- invadeFinal v
  return vs

successProb :: Battlefield -> Rand StdGen Double
successProb bf = do
  v <- replicateM 100000 (invadeFinal bf)
  let
    scores = winLose <$> v
    probs = avgList scores
  return probs
    

  --avgList <*> replicateM 1000 (invadeFinal bf)

winLose :: Battlefield -> Double
winLose (Battlefield 1 x) = 0
winLose (Battlefield x 0) = 1

avgList :: [Double] -> Double
avgList xs = (foldr1 (+) xs)/(fromIntegral (length xs))

main = do
  bff2 <- evalRandIO (invadeFinal (Battlefield 15 15))
  prob <- evalRandIO (successProb (Battlefield 3 3))
  putStrLn (show prob)

--JMJ DG
