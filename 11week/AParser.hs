
import Control.Applicative
import Data.Char
import Data.Maybe
import Data.Monoid

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing
    f (x:xs)
      | p x = Just (x, xs)
      | otherwise = Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

char2 :: Parser Char
char2 = satisfy (isUpper)

first :: (a -> b) -> (a,c) -> (b,c)
first f tuple = ((f (fst tuple)),(snd tuple))

instance Functor Parser where
  fmap f (Parser g) = (Parser (fmap (first f) . g))

-- the type of f is (String -> Maybe (a -> b, String))
-- JMJ, please help me to understand why this works
-- https://www.seas.upenn.edu/~cis194/spring13/hw/10-applicative.pdf


instance Applicative Parser where
  pure a = (Parser (\x -> Just (a, x)))
  (Parser f) <*> (Parser g) = Parser $ \x ->
    case f x of
    Nothing -> Nothing
    Just (function, x') -> runParser (fmap function (Parser g)) x' -- ok this is cool as hell, you can introduce entirely new stuff

posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

abParser :: Parser (Char,Char)
abParser = Parser f
  where
    f (x:y:xs)
      | (x == 'a') && (y == 'b') = Just ((x,y), xs)
      | otherwise = Nothing

abParser_ :: Parser ()
abParser_ = Parser f
  where
    f (x:y:xs)
      | (x == 'a') && (y == 'b') = Just ((), xs)
      | otherwise = Nothing

abParser1 :: Parser (Char,Char)
abParser1 = (\x y -> (x,y)) <$> char 'a' <*> char 'b'

abParser_1 :: Parser ()
abParser_1 = (\x y -> ()) <$> char 'a' <*> char 'b'

intPair :: Parser [Integer]
intPair = (\x _ y -> [x, y]) <$> posInt <*> char ' ' <*> posInt

newPosInt :: Parser ()
newPosInt = (\x -> ()) <$> posInt

newChar :: Parser ()
newChar = (\x -> ()) <$> char2

-- Exercise 4

instance Alternative Parser where
  empty = (Parser $ \x -> Nothing)
  (Parser l) <|> (Parser r) = Parser $ \x ->
    case l x of
    Nothing -> r x
    Just (t,x') -> Just (t,x')

intOrUppercase = newPosInt <|> newChar
