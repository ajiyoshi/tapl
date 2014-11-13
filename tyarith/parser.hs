module Parser where

import Text.Parsec
import Text.Parsec.String
import Control.Applicative hiding ((<|>))

import qualified Lex as L
import Core

{-
 - term := appTerm | If term Then term Else term
 - appTerm := aTerm | Succ aTerm | Pred aTerm | Iszero aTerm
 - aTerm = LPAREN term RPAREN | True | False | IntV
 -}

term :: Parser Term
term = appTerm <|> ifThenElse

appTerm :: Parser Term
appTerm = aTerm
  <|> do
    L.reserved "succ"
    t <- term
    return $ TmSucc t
  <|> do
    L.reserved "pred"
    t <- term
    return $ TmPred t
  <|> do
    L.reserved "iszero"
    t <- term
    return $ TmIszero t

aTerm :: Parser Term
aTerm =
  L.parens term
  <|> do
    L.reserved "true"
    return TmTrue
  <|> do
    L.reserved "false"
    return TmFalse
  <|> do
    n <- L.natural
    return $ TmNat n

ifThenElse :: Parser Term
ifThenElse =
  do
    L.reserved "if"
    t1 <- term
    L.reserved "then"
    t2 <- term
    L.reserved "else"
    t3 <- term
    return $ TmIf t1 t2 t3

runTypeof :: String -> Either String Type
runTypeof s = case parse term "ERROR" s of
  Left msg -> fail $ show msg
  Right t -> typeof t
