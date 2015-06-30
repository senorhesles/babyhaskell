{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module JoinList where

import Data.Monoid
import Buffer
import Editor

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)

--instance Monoid (JoinList m a) where
--  mempty = Empty
--  mappend = (+++)

test :: JoinList m a
test = Empty

test2 :: JoinList [Integer] Integer
test2 = Single [5] 5

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







