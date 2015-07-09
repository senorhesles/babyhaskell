module Scrabble where

import Data.Monoid

data Score = Score Int deriving (Eq, Show)

instance Monoid Score where
  mempty = Score 0
  mappend = addingScores
  
addingScores :: Score -> Score -> Score
addingScores (Score x) (Score y) = Score (x + y)

scoreString :: [Char] -> Score
scoreString x = foldr1 addingScores (map score x)

score :: Char -> Score
score x
  | x == 'a' = Score 1
  | x == 'A' = Score 1
  | x == 'b' = Score 3
  | x == 'B' = Score 3
  | x == 'c' = Score 3
  | x == 'C' = Score 3
  | x == 'd' = Score 2
  | x == 'D' = Score 2
  | x == 'e' = Score 1
  | x == 'E' = Score 1
  | x == 'f' = Score 4
  | x == 'F' = Score 4
  | x == 'g' = Score 2
  | x == 'G' = Score 2
  | x == 'h' = Score 4
  | x == 'H' = Score 4
  | x == 'i' = Score 1
  | x == 'I' = Score 1
  | x == 'j' = Score 8
  | x == 'J' = Score 8
  | x == 'k' = Score 5
  | x == 'K' = Score 5
  | x == 'l' = Score 1
  | x == 'L' = Score 1
  | x == 'm' = Score 3
  | x == 'M' = Score 3
  | x == 'n' = Score 1
  | x == 'N' = Score 1
  | x == 'o' = Score 1
  | x == 'O' = Score 1
  | x == 'p' = Score 3
  | x == 'P' = Score 3
  | x == 'q' = Score 10
  | x == 'Q' = Score 10
  | x == 'r' = Score 1
  | x == 'R' = Score 1
  | x == 's' = Score 1
  | x == 'S' = Score 1
  | x == 't' = Score 1
  | x == 'T' = Score 1
  | x == 'u' = Score 1
  | x == 'U' = Score 1
  | x == 'v' = Score 4
  | x == 'V' = Score 4
  | x == 'w' = Score 4
  | x == 'W' = Score 4
  | x == 'x' = Score 8
  | x == 'X' = Score 8
  | x == 'y' = Score 4
  | x == 'Y' = Score 4
  | x == 'z' = Score 10
  | x == 'Z' = Score 10
  | otherwise = Score 0

