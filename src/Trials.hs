module Trials where

import Data.Maybe
import Text.ParserCombinators.Parsec

data TExpr = TList [TExpr]
  | TInt Int
  | TFloat Float
  | TSpace
  | TSymbol String deriving (Show, Eq)

filterTExprs :: TExpr -> Maybe TExpr
filterTExprs (TList xs) = Just $ TList $ catMaybes $ map filterTExprs xs
filterTExprs TSpace = Nothing
filterTExprs x = Just x

trials :: Parser [TExpr]
trials = many $
  number
  <|> symbol
  <|> list
  <|> whitespace

symbol :: Parser TExpr
symbol = do
  f <- symbolChar
  s <- many symbolChar
  return $ TSymbol (f:s)
  where
  symbolChar = letter <|> digit <|> sym

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
list = do
  char '('
  contents <- trials
  char ')'
  return $ TList contents

parseText text = case parse (trials) "" text of
  (Right exprs) -> Right $ catMaybes $ map filterTExprs exprs
  err -> err
