
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = (fib (n - 1)) + (fib (n - 2))

fibs1 :: [Integer]
fibs1 = [0] ++ map fib [1..]

fib2 :: [Integer] -> [Integer]
fib2 xs = xs ++ [((last xs) + (last $ init xs))]

fibs2' :: [Integer]
fibs2' = fibgen 0 1 where
  fibgen a b = a : fibgen b (a + b)

data Stream a = Cons a (Stream a) deriving Show

test :: Stream Integer
test = Cons 5 test

streamToList :: Stream a -> [a]
streamToList (Cons x _)  = repeat x

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x y) = Cons (f x) (streamMap f y)

streamToThing :: Stream a -> a
streamToThing (Cons x _) = x

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons (f x) (streamFromSeed f (f x))

nats :: Stream Integer
nats = streamFromSeed (+1) 0

natsNoZero :: Stream Integer
natsNoZero = streamFromSeed (+1) 1

ruler :: Stream Integer
ruler = streamMap f natsNoZero where
  f = (\x -> if ((x `mod` 2^(x-1)) = 0) && ((x `div` 2^(x-1)) = 1) && ((x `mod` 2^x) /= 0) then 

--ruler' :: Stream Integer
--ruler' = streamMap f natsNoZero where
--  f = (\x -> 
