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
string2Log ("I":y:zs) = LogMessage Info (read y) (unwords zs)
string2Log ("W":y:zs) = LogMessage Warning (read y) (unwords zs)
string2Log ("E":y:z:zs) = LogMessage (Error (read y)) (read z) (unwords zs)
string2Log _ = Unknown "random"

--ex2

data Treey = Leafy Char | Nodey Treey Int Treey

tree :: Treey
tree = Nodey (Leafy 'x') 1 (Nodey (Leafy 'y') 2 (Leafy 'z'))

messagetree :: MessageTree
messagetree = Node
              (Node
               Leaf
               (LogMessage Warning 51 "sup")
               Leaf)
              (LogMessage Warning 52 "hello")
              (Node
               Leaf
               (LogMessage Warning 53 "hi")
               Leaf)

--insert :: LogMessage -> MessageTree -> MessageTree
--insert x (Leaf) = (Node Leaf x Leaf)

insert' :: LogMessage -> MessageTree -> MessageTree
insert' x (Node z y w)
  | (log2Time x) > (log2Time y) = (Node z y (insert' x w))
  | (log2Time x) < (log2Time y) = (Node (insert' x z) y w)
  | otherwise = (Node z y w) 
insert' (Unknown _) y = y
insert' x _ = (Node Leaf x Leaf)

{- the reason you were having trouble is because you thought that >, <, and == were
logically exhaustive....they typically are, but the thing is, is GHC has no guarantee
that (log2Time x) or (log2Time y) are going to fail to have the proper type...and if
they fail to have the proper type...well. its neither < nor > nor ==...so in the event
it fails we will return the original Tree -}
                                  
log2Time :: LogMessage -> TimeStamp
log2Time (LogMessage _ n _) = n
log2Time (Unknown _) = 0

tree2Time :: MessageTree -> TimeStamp
tree2Time (Node _ n _) = log2Time n
tree2Time (Leaf) = 0

tree2Log :: MessageTree -> LogMessage
tree2Log (Node _ n _) = n
tree2Log (Leaf) = (Unknown "what")
