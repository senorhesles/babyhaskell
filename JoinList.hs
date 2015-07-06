{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module JoinList where

import Data.Monoid
import Buffer
import Editor
import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)

test :: JoinList Size String
test = Append (Size 2) (Single (Size 1) "This is the first full line") (Single (Size 1) "This is the second full line")

test2 :: JoinList Size String
test2 = Append (Size 4) (Append (Size 3) (Append (Size 2) (Single (Size 1) "First") (Single (Size 1) "Second")) (Single (Size 1) "Third")) (Single (Size 1) "Fourth")

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty x = x
(+++) x Empty = x
(+++) (Single x y) (Single z u) = Append (mappend x z) (Single x y) (Single z u)
(+++) (Single x y) (Append m (z) (u)) = Append (mappend x m) (Single x y) (Append m (z) (u))
(+++) (Append m (z) (u)) (Single x y) = Append (mappend m x) (Append m (z) (u)) (Single x y)
(+++) (Append m (x) (y)) (Append n (z) (u)) = Append (mappend m n) (Append m (x) (y)) (Append n (z) (u))

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m a) = m
tag (Append m _ _) = m

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ int _ | int < 0 = Nothing
indexJ int Empty = Nothing
indexJ int (Single m a) = Just a
indexJ int (Append m x y)
  | (getSize (size m)) == int = indexJ int y
  | otherwise = (indexJ int x)

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ int Empty = Empty
dropJ 0 x = x
dropJ x (Single m a) | x > 0 = Empty
dropJ int (Append m x y)
  | (getSize (size m)) > int = dropJ int x +++ y
  | otherwise = y

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ int Empty = Empty
takeJ 0 x = Empty
takeJ int (Single m a) | int > 0 = (Single m a)
takeJ int (Append m x y)
  | int == (getSize (size m)) = (Append m x y)
  | int > (getSize (size m)) = Empty
  | int < (getSize (size m)) = takeJ int x

