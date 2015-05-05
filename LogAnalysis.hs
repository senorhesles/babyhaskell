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

insert' :: LogMessage -> MessageTree -> MessageTree
insert' x (Node z y w)
  | (log2Time x) > (log2Time y) = (Node z y (insert' x w))
  | (log2Time x) < (log2Time y) = (Node (insert' x z) y w)
  | otherwise = (Node z y w) 
insert' (Unknown _) y = y
insert' x _ = (Node Leaf x Leaf)

log2Time :: LogMessage -> TimeStamp
log2Time (LogMessage _ n _) = n
log2Time (Unknown _) = 0

tree2Log :: MessageTree -> LogMessage
tree2Log (Node _ n _) = n
tree2Log (Leaf) = (Unknown "what")

build :: [LogMessage] -> MessageTree
build (x:xs) = insert' x (build xs)
build [] = (Leaf)

inOrder :: MessageTree -> [LogMessage]
inOrder x = (tree2Log x):(inOrder x)

tree2Time :: MessageTree -> TimeStamp
tree2Time (Node _ n _) = log2Time n
tree2Time (Leaf) = 0

onlyErrors :: [LogMessage] -> [LogMessage]
onlyErrors [(LogMessage (Error x) y z)] = [(LogMessage (Error x) y z)]
onlyErrors [(LogMessage Info _ _)] = []
onlyErrors [(LogMessage Warning _ _)] = []
onlyErrors _ = []

errorsAbove :: [LogMessage] -> [LogMessage]
errorsAbove [(LogMessage (Error x) y z)]
             | x > 50 = [(LogMessage (Error x) y z)]
             | x < 50 = []
             | otherwise = []
errorsAbove _ = []

sortByTimeGetText :: [LogMessage] -> [String]
sortByTimeGetText ((LogMessage _ x z):(LogMessage _ y w):u)
  | x < y = [z,w] ++ (sortByTimeGetText u)
  | y > x = [w,z] ++ (sortByTimeGetText u)
  | otherwise = []
sortByTimeGetText _ = []

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong x = sortByTimeGetText $ errorsAbove $ (onlyErrors x)

-- Ok, take List of Logs, get those with E 50 or more, put them in new list, sort them by time
-- stamp, replace them with their corresponding Strings

-- Where we're at is...we can't filter for Errors...or by Errors of a particular severity. In addition, we don't know how to order a Message Tree based on timestamp
