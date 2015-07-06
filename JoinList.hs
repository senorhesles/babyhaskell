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

--instance Monoid (JoinList m a) where
--  mempty = Empty
--  mappend = (+++)

test :: JoinList m a
test = Empty

test2 :: JoinList Size Char
test2 = Append (Size 210)
        (Append (Size 30)
         (Single (Size 5) 'y')
         (Append (Size 6)
          (Single (Size 2) 'e')
          (Single (Size 3) 'a')))
        (Single (Size 7) 'h')

test3 :: JoinList Size Char
test3 = Append (Size 2) (Single (Size 1) 'n') (Single (Size 2) 'o')

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
indexJ int (Single m a)
  | (getSize (size m)) == int = Just a
  | otherwise = Nothing
indexJ int (Append m x y)
  | (getSize (size (tag y))) == int = indexJ int y
  | (getSize (size (tag y))) > int = indexJ int x

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

safeListIndex :: [a] -> Int -> Maybe a
safeListIndex [] _ = Nothing
safeListIndex _ i | i <= 0 = Nothing
safeListIndex (x:xs) 1 = Just x
safeListIndex (x:xs) i = safeListIndex xs (i-1)

-- Couldn't make the indices match up; it was a data structure problem

dropJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a

dropJ int jl | int <= 0 = jl
dropJ int Empty = Empty
dropJ int (Single m a) = Empty
dropJ int (Append m (Single x u) y) = dropJ (int - 1) y

takeJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
takeJ int jl | int <= 0 = jl
takeJ _ Empty = Empty
takeJ _ (Single m a) = Single m a
takeJ int (Append m x y) = x +++ (takeJ (int - 1

-- Ok...we need to come up with a data structure, specifically, we need to figure
-- out how the JoinLists are structured, and how, or whether, the annotations
-- have any relation to where in the JoinList that particular piece of data is.
