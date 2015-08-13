
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

instance Applicative Parser where
  pure a = Parser (\x -> Just (a, x))
  (Parser p1) <*> (Parser p2) =
    let
      function = fst (Parser p1)
      string = fmap snd (Parser p1)
    in (Parser function (p2 string))
      

-- (first p1) takes as inputs tuples

type Name = String

data Employee = Employee { name    :: Name
                         , phone   :: String }
              deriving Show

m_name1, m_name2 :: Maybe Name
m_name1 = Nothing
m_name2 = Just "Brent"

m_phone1, m_phone2 :: Maybe String
m_phone1 = Nothing
m_phone2 = Just "555-1234"

ex01 = Employee <$> m_name1 <*> m_phone1
ex02 = Employee <$> m_name1 <*> m_phone2
ex03 = Employee <$> m_name2 <*> m_phone1
ex04 = (Employee <$> m_name2) <*> m_phone2

-- the type of f is (String -> Maybe (a -> b, String))
-- JMJ, please help me to understand why this works
-- https://www.seas.upenn.edu/~cis194/spring13/hw/10-applicative.pdf

