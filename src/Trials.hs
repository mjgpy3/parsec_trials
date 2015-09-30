module Trials where

import Data.Maybe
import Text.ParserCombinators.Parsec hiding (string)

data TExpr = TList [TExpr]
  | TInt Int
  | TFloat Float
  | TSpace
  | TSymbol String
  | TString String deriving (Show, Eq)

filterTExprs :: TExpr -> Maybe TExpr
filterTExprs (TList xs) = Just $ TList $ catMaybes $ map filterTExprs xs
filterTExprs TSpace = Nothing
filterTExprs x = Just x

trials :: Parser [TExpr]
trials = many $
  string
  <|> try number
  <|> symbol
  <|> list
  <|> whitespace

string :: Parser TExpr
string = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ TString x

symbol :: Parser TExpr
symbol = do
  let symbolChar = letter <|> digit <|> sym
  f <- symbolChar
  s <- many symbolChar
  return $ TSymbol (f:s)

sym :: Parser Char
sym = oneOf "!$%&|*+-/:<=>?@^_~#"

whitespace :: Parser TExpr
whitespace = do
  char ' '
  return TSpace

number :: Parser TExpr
number = try float
  <|> try negativeFloat
  <|> negativeInt
  <|> int

negativeFloat :: Parser TExpr
negativeFloat = do
  char '-'
  TFloat f <- float
  return $ TFloat $ negate f

float :: Parser TExpr
float = do
  w <- many1 digit
  char '.'
  f <- many1 digit
  return $ TFloat $ (read (w ++ "." ++ f) :: Float)

negativeInt :: Parser TExpr
negativeInt = do
  char '-'
  TInt i <- int
  return $ TInt $ negate i

int :: Parser TExpr
int = do
  t <- many1 digit
  return $ TInt $ (read t :: Int)

list :: Parser TExpr
list = listWith '(' ')'
  <|> listWith '[' ']'

listWith :: Char -> Char -> Parser TExpr
listWith open close = do
  char open
  contents <- trials
  char close
  return $ TList contents

parseText = fmap (catMaybes . map filterTExprs) . parse (trials) ""
