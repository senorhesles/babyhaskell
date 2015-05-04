{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where


import Prelude
import Log

-- ex1

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

build :: [LogMessage] -> MessageTree
build (x:xs) = insert' x (build xs)
build [] = (Leaf)

inOrder :: MessageTree -> [LogMessage]
inOrder x = (tree2Log x):(inOrder x)

--whatWentWrong :: [LogMessage] -> [String]
--whatWentWrong (x:xs) =



-- Ok the trouble is...I need to get an ErrorInt...but not all MessageTypes have ErrorInts
-- So my function isn't going to be able to be total. I won't be able to go from any LogMessage
-- to an ErrorInt...only some will have an output...I could just have the rest all go to zero
-- but that feels so cheap

-- Ok ...JMJ...I figured it out...with help...I can specify that its a function that only
-- takes as inputs (LogMessage (Error ErrorInt))...derp...thought it could only be from
-- LogMessage

-- Or not...


--log2ErrorInt' (LogMessage (Error x)_ _) = x

--log2ErrorInt :: LogMessage -> ErrorInt
--log2ErrorInt (LogMessage (Error x) _ _) = x

log2MaybeErrorInt :: LogMessage -> Maybe ErrorInt
log2MaybeErrorInt (LogMessage (Error x) _ _) = Just x
log2MaybeErrorInt _ = Nothing

maybeErrorInt2ErrorInt :: Maybe ErrorInt -> ErrorInt
maybeErrorInt2ErrorInt (Just x) = x
maybeErrorInt2ErrorInt Nothing = 0

log2ErrorInt :: LogMessage -> ErrorInt
log2ErrorInt x = maybeErrorInt2ErrorInt $ log2MaybeErrorInt x

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong (x:xs) =


-- Ok, take List of Logs, get those with E 50 or more, put them in new list, sort them by time
-- stamp, replace them with their corresponding Strings
