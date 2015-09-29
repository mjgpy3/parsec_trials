module Trials where

import Text.ParserCombinators.Parsec

data TExpr = TList [TExpr]
  | TInt Int
  | TFloat Float deriving (Show, Eq)

trials :: Parser TExpr
trials = number
  <|> list

number :: Parser TExpr
number = try float
  <|> negativeInt
  <|> int

float :: Parser TExpr
float = do
  w <- many1 digit
  char '.'
  f <- many1 digit
  return $ TFloat $ (read (w ++ "." ++ f) :: Float)

negativeInt :: Parser TExpr
negativeInt = do
  char '-'
  t <- many1 digit
  return $ TInt $ (read ('-':t) :: Int)

int :: Parser TExpr
int = do
  t <- many1 digit
  return $ TInt $ (read t :: Int)

list :: Parser TExpr
list = do
  char '('
  char ')'
  return $ TList []

parseText = parse trials ""
