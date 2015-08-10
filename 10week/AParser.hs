
import Control.Applicative
import Data.Char
import Data.Maybe

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

posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

first :: (a -> b) -> (a,c) -> (b,c)
first f tuple = ((f (fst tuple)),(snd tuple))

instance Functor Parser where
  fmap f (Parser g) = (Parser (fmap (first f) . g))

-- JMJ, please help me to understand why this works
-- https://www.seas.upenn.edu/~cis194/spring13/hw/10-applicative.pdf
