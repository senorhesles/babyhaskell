{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Prelude
import Log

parse :: String -> [LogMessage]
parse x = strList2LogList $ lines x

strList2LogList :: [String] -> [LogMessage]
strList2LogList (x:xs) = (parseMessage x) : (strList2LogList xs)
strList2LogList [] = []

parseMessage :: String -> LogMessage
parseMessage x = string2Log $ words x

string2Log :: [String] -> LogMessage
string2Log ("E":y:z:zs) = LogMessage (Error (read y)) (read z) (unwords zs)
string2Log ("I":y:zs) = LogMessage Info (read y) (unwords zs)
string2Log ("W":y:zs) = LogMessage Warning (read y) (unwords zs)
string2Log _ = Unknown "random"

build :: [LogMessage] -> MessageTree -- checks out
build (x:xs) = insert x (build xs)
build [] = (Leaf)

insert :: LogMessage -> MessageTree -> MessageTree -- checks out
insert x (Node z y w)
  | (log2Time x) > (log2Time y) = (Node z y (insert x w))
  | (log2Time x) < (log2Time y) = (Node (insert x z) y w)
  | otherwise = (Node z y w) 
insert (Unknown _) y = y
insert x _ = (Node Leaf x Leaf)

log2Time :: LogMessage -> TimeStamp
log2Time (LogMessage _ n _) = n
log2Time (Unknown _) = 0

inOrder :: MessageTree -> [LogMessage]
inOrder (Node m n o) = (inOrder m) ++ [n] ++ (inOrder o)
inOrder (Leaf) = []

errorsAbove :: [LogMessage] -> [LogMessage]
errorsAbove ((LogMessage (Error x) y z):ls)
  | x > 25 = ((LogMessage (Error x) y z):(errorsAbove ls))
  | x < 25 = [] ++ (errorsAbove ls)
  | otherwise = [(LogMessage Warning 59 "otherwise")] ++ (errorsAbove ls)
errorsAbove ((LogMessage Info _ _):ls) = [] ++ (errorsAbove ls)
errorsAbove ((LogMessage Warning _ _):ls) = [] ++ (errorsAbove ls)
errorsAbove ((Unknown _):ls) = [] ++ (errorsAbove ls)
errorsAbove _ = []


sortByTimeGetText :: [LogMessage] -> [String]
sortByTimeGetText ((LogMessage _ x z):(LogMessage _ y w):u)
  | x < y = [z,w] ++ (sortByTimeGetText u)
  | y > x = [w,z] ++ (sortByTimeGetText u)
  | otherwise = [] ++ (sortByTimeGetText u)
sortByTimeGetText _ = []

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong x = sortByTimeGetText $ errorsAbove x

-- JMJ

