import AParser
import Control.Applicative
import Data.Char

<<<<<<< HEAD
{-
parseSExpr :: Parser SExpr
parseSExpr = Parser f
  where
    f xs
      | null xs = Nothing
      | 
-}

test :: Atom
test = N 5
test2 :: Atom
test2 = I "foobar"
test3 :: SExpr
test3 = A (N 5)
test4 :: SExpr
test4 = Comb [(A (N 5)), (A (I "string"))]
test5 :: SExpr
test5 = Comb [(A (N 5)), (A (I "string")), (Comb [(A (N 6)), (A (I "more"))])]
=======
-- Exercise 1
>>>>>>> f19cb5539e509b32f83ee5b498d008575f2a0ed6

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p -- so apparently you can define functions in tandem like this

-- Exercise 2

spaces :: Parser String
spaces = zeroOrMore . satisfy $ isSpace

ident :: Parser String
ident = (:) <$> (satisfy isAlpha) <*> (zeroOrMore . satisfy $ isAlphaNum)

<<<<<<< HEAD
parens :: Parser a -> Parser a
parens p = char '(' *> p <* char ')'

parseAtom :: Parser Atom
parseAtom = N <$> posInt <|> I <$> ident -- Remember that N and I are type constructors...you can use them before fmap and a parser to get a new parser

parseSExpr :: Parser SExpr
parseSExpr = spaces *> (A <$> parseAtom <|> Comb <$> (oneOrMore (parens parseSExpr))) <* spaces

type Ident = String
data Atom = N Integer | I Ident deriving Show
data SExpr = A Atom | Comb [SExpr] deriving Show
=======
-- Exercise 3





--JMJ
>>>>>>> f19cb5539e509b32f83ee5b498d008575f2a0ed6
