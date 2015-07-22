--main = putStrLn "Hello, Haskell!"

-- putStrLn :: String -> IO ()
-- go to terminal and type:
-- > runhaskell lecture.hs

-- or go to terminal and type ghc --make lecture.hs

-- a value of type IO String is just a description of some computation
-- It's a recipe for generating something of type 'a', nothing of type 'a'
-- exists at all yet.

-- COMBINING IO OPERATIONS

-- (>>) is parsed as 'and then'; it has the following type:
-- (>>) :: IO a -> IO b -> IO b
-- result of first computation is thrown away...all we care about are optional effects

--main = putStrLn "Hello" >> putStrLn "world!"

-- It would be nice to be able to have something to where the subsequent computations
-- could depend upon the results of the previous computations
-- Luckily haskell dudes were smart, and gave us this

-- (>>=) is pronounced "bind"
-- (>>=) :: IO a -> (a -> IO b) -> IO b

data De = Ce Integer String Integer

main :: IO ()
main = putStrLn "Please enter a number: " >> (readLn >>= (\n ->
                                                           putStrLn (
                                                             show (n+1))))

-- RECORD SYNTAX FOR DATA TYPES



-- could have been defined as

data Dee = Cee { field1 :: Integer, field2 :: String, field3 :: Integer }


