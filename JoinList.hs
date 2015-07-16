{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module JoinList where

import Data.Monoid
import Buffer
import Editor
import Sized
import Scrabble
{-
data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)

test :: JoinList Size String
test = Append (Size 2) (Single (Size 1) "This is the first full line") (Single (Size 1) "This is the second full line")
test2 :: JoinList Size String
test2 = Append (Size 4)
        (Append (Size 3)
         (Append (Size 2)
          (Single (Size 1) "First")
          (Single (Size 1) "Second"))
         (Single (Size 1) "Third"))
        (Single (Size 1) "Fourth")
test3 :: JoinList (Score, Size) String
test3 = stringToJL "mario"
test4 = stringToJL "this is the second sentence"
t6 = stringToJL "third"
t7 = stringToJL "fourth"
--test5 = test3 +++ test4 +++ t6 +++ t7

--(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
--(+++) Empty x = x
--(+++) x Empty = x
--(+++) (Single x y) (Single z u) = Append (mappend x z) (Single x y) (Single z u)
--(+++) (Single x y) (Append m z u) = Append (mappend x m) ((Single x y) +++ z) +++ u
--(+++) (Append m z u) (Single x y) = Append (mappend m x) (Append m z u) (Single x y)
--(+++) (Append m x y) (Append n z u) = Append (mappend m n) (Append m x y) (Append n z u)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty x = x
(+++) x Empty = x
(+++) x y = Append (mappend (tag x) (tag y)) x y

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m a) = m
tag (Append m _ _) = m

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJorig int _ | int < 0 = Nothing
indexJorig int Empty = Nothing
indexJorig int (Single m a) = Just a
indexJorig int (Append m x y)
  | (getSize (size m)) <= (int + 1) = indexJorig int y
  | otherwise = (indexJorig int x)

indexJ2 :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ2 _ Empty                      = Nothing
indexJ2 i0 (Single _ a0)             = if i0 == 0 then Just a0 else Nothing
indexJ2 i0 (Append m0 jl0 jl1)
    | i0 < 0 || i0 > centerSize     = Nothing
    | i0 < leftSize                 = indexJ2 (leftSize - i0) jl0
    | otherwise                     = indexJ2 (i0 - leftSize) jl1
  where
    leftSize = getSize $ size $ tag jl0
    centerSize = getSize $ size m0

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ 0 (Single m a) = Just a
indexJ _ (Single m a) = Nothing
indexJ i (Append v m n)
  | i < 0 = Nothing
  | i < split = indexJ i m
  | otherwise = indexJ (i - split) n
  where split = getSize . size . tag $ m

dropJ2 :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ2 int Empty = Empty
dropJ2 0 x = x
dropJ2 x (Single m a) | x > 0 = Empty
dropJ2 int (Append m x y)
  | (getSize (size m)) > int = dropJ2 int x +++ y
  | otherwise = y

takeJ2 :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ2 int Empty = Empty
takeJ2 int (Single m a)
  | int > 0 = (Single m a)
  | otherwise = Empty
takeJ2 int (Append m x y)
  | int == (getSize (size m)) = (Append m x y)
  | otherwise = takeJ int x


scoreLine :: String -> JoinList Score String
scoreLine x = Single (scoreString x) x

jlToString :: JoinList m String -> String
jlToString Empty = ""
jlToString (Single m a) = a
jlToString (Append m x y) = ""

stringToJL :: String -> JoinList (Score, Size) String
stringToJL x = (Single ((scoreString x), (Size 1)) x)

jlToSize :: JoinList (Score, Size) a -> Int
jlToSize Empty = 0
jlToSize (Single _ _) = 1
jlToSize (Append (_, Size x) _ _) = x

jlToScore :: JoinList (Score, Size) a -> Int
jlToScore Empty = 0
jlToScore (Single (Score x, _) _) = x
jlToScore (Append (Score x, _) _ _) = x

instance Buffer (JoinList (Score, Size) String) where
  toString = jlToString
  fromString = stringToJL
  line num jl = indexJ num jl
  replaceLine num str jl = (takeJ (num - 1) jl) +++ (stringToJL str) +++ (dropJ (num + 1) jl) -- might need to mess with this
  numLines = jlToSize
  value = jlToScore

-- I have a feeling the concat function is messing with us...the thing is...is
-- indexJ doesn't index the correct dude

-}

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty jl = jl
(+++) jl Empty = jl
(+++) m n = Append (tag m <> tag n) m n

tag :: Monoid m => JoinList m a -> m
tag (Single m a) = m
tag (Append m _ _) = m

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ 0 (Single m a) = Just a
indexJ _ (Single m a) = Nothing
indexJ i (Append v m n)
  | i < 0 = Nothing
  | i < split = indexJ i m
  | otherwise = indexJ (i - split) n
  where split = getSize . size . tag $ m

dropJ:: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ 0 jl = jl
dropJ _ (Single _ _) = Empty
dropJ i l@(Append v m n)
  | i <= 0 = l
  | i >= split = (dropJ (i - split) n)
  | otherwise = (dropJ i m) +++ n
  where split = getSize . size . tag $ m
                
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ 0 _ = Empty
takeJ _ Empty = Empty
takeJ _ s@(Single _ _) = s
takeJ i l@(Append v m n)
  | i <= 0 = Empty
  | i >= split = m +++ (takeJ (i - split) n)
  | otherwise = (takeJ i m)
  where split = getSize . size . tag $ m

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

instance Buffer (JoinList (Score, Size) String) where
  toString Empty = ""
  toString (Single _ s) = s
  toString (Append _ m n) = toString m ++ toString n
  fromString s = Single ((scoreString s), Size 1) s
  line n jl    = indexJ n jl
  replaceLine n l b = takeJ (n-1) b +++ (fromString l) +++ dropJ n b
  numLines Empty = 0
  numLines (Single (sc, si) _)   = getSize si
  numLines (Append (sc, si) _ _) = getSize si
  value = jlToScore

jlToScore :: JoinList (Score, Size) a -> Int
jlToScore Empty = 0
jlToScore (Single (Score x, _) _) = x
jlToScore (Append (Score x, _) _ _) = x

test3 :: JoinList (Score, Size) String
test3 = stringToJL "mario"

stringToJL :: String -> JoinList (Score, Size) String
stringToJL x = (Single ((scoreString x), (Size 1)) x)
