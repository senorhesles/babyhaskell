{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Prelude
import ExprT
import Parser
import Data.List

newtype MinMax = MinMax Integer deriving (Eq, Show)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)


class Expr a where
  mul :: a -> a -> a
  add :: a -> a -> a
  lit :: Integer -> a

  
instance Expr ExprT where
  mul x y = Mul x y
  add x y = Add x y
  lit x = Lit x

instance Expr Integer where
  lit x = x
  add x y = x + y
  mul x y = x * y

instance Expr Bool where
  lit = (\x -> if x > 0 then True else False)
  add = max
  mul = min

instance Expr MinMax where
  lit x = MinMax x
  add (MinMax x) (MinMax y) = MinMax (max x y)
  mul (MinMax x) (MinMax y) = MinMax (min x y)

instance Expr Mod7 where
  lit x = Mod7 (x `mod` 7)
  add (Mod7 x) (Mod7 y) = Mod7 ((x + y) `mod` 7)
  mul (Mod7 x) (Mod7 y) = Mod7 ((x * y) `mod` 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7
  

reify :: ExprT -> ExprT
reify = id

eval :: ExprT -> Integer
eval (Mul x y) = (eval x) * (eval y)
eval (Add x y) = (eval x) + (eval y)
eval (Lit x) = x

justEval :: Maybe ExprT -> Maybe Integer
justEval Nothing = Nothing
justEval (Just x) = Just (eval x)


evalStr :: String -> Maybe Integer
evalStr x =
  let
    parsed = parseExp Lit Add Mul x
  in
   justEval parsed





