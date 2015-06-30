import Data.Monoid
import Data.Foldable
import Control.Monad.Writer
import Control.Monad.State

fact1 :: Integer -> Writer String Integer
fact1 0 = return 1
fact1 n = do
  let n' = n-1
  tell $ "We've taken one away from " ++ show n ++ "\n"
  m <- fact1 n'
  tell $ "We've called f " ++ show m ++ "\n"
  let r = n*m
  tell $ "We've multiplied " ++ show n ++ " and " ++ show m ++ "\n"
  return r

ex1 = runWriter (fact1 4)


fact2 :: Integer -> Writer (Sum Integer) Integer
fact2 0 = return 1
fact2 n = do
  let n' = n-1
  tell $ Sum 1
  m <- fact2 n'
  let r = n*m
  tell $ Sum 1
  return r

ex2 = runWriter (fact2 10)

fact3 :: Integer -> State Integer Integer
fact3 0 = return 1
fact3 n = do
  let n' = n-1
  modify (+1)
  m <- fact3 n'
  let r = n*m
  modify (+1)
  return r

  
fact4 :: Integer -> Writer Any Integer
fact4 0 = return 1
fact4 n = do
   let n' = n-1
   m <- fact4 n'
   let r = n*m
   tell (Any (r==120))
   return r

ex4 = runWriter (fact4 10)
  
  
