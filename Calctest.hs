--{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import Prelude
import qualified ExprT as Ex
import Parser
import Data.List
import qualified StackVM as Stack

newtype MinMax = MinMax Integer deriving (Eq, Show)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)


class Expr a where
  mul :: a -> a -> a
  add :: a -> a -> a
  lit :: Integer -> a

  
instance Expr Ex.ExprT where
  mul x y = Ex.Mul x y
  add x y = Ex.Add x y
  lit x = Ex.Lit x

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

instance Expr Stack.Program where
  lit x = [Stack.PushI x]
  add x y = x ++ y ++ [Stack.Add]
  mul x y = x ++ y ++ [Stack.Mul]
    
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

reify :: Ex.ExprT -> Ex.ExprT
reify = id

eval :: Ex.ExprT -> Integer
eval (Ex.Mul x y) = (eval x) * (eval y)
eval (Ex.Add x y) = (eval x) + (eval y)
eval (Ex.Lit x) = x

justEval :: Maybe Ex.ExprT -> Maybe Integer
justEval Nothing = Nothing
justEval (Just x) = Just (eval x)


evalStr :: String -> Maybe Integer
evalStr x =
  let
    parsed = parseExp Ex.Lit Ex.Add Ex.Mul x
  in
   justEval parsed
