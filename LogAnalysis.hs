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
