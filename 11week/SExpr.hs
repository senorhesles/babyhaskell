
import AParser
import Control.Applicative
import Data.Char


zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p -- so apparently you can define functions in tandem like this

spaces :: Parser String
spaces = zeroOrMore . satisfy $ isSpace

ident :: Parser String
ident = (:) <$> (satisfy isAlpha) <*> (zeroOrMore . satisfy $ isAlphaNum)
