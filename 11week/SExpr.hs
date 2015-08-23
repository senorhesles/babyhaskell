
import AParser
import Control.Applicative
import Data.Char

-- Exercise 1

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p -- so apparently you can define functions in tandem like this

-- Exercise 2

spaces :: Parser String
spaces = zeroOrMore . satisfy $ isSpace

ident :: Parser String
ident = (:) <$> (satisfy isAlpha) <*> (zeroOrMore . satisfy $ isAlphaNum)

-- Exercise 3





--JMJ
