module Lex where

import Text.Parsec.Language
import qualified Text.Parsec.Token as P
import Text.Parsec.String


lexer :: P.TokenParser ()
lexer = P.makeTokenParser (javaStyle {
        P.reservedNames = ["lambda"]
        , P.reservedOpNames = ["^"]
        } )

reserved :: String -> Parser ()
reserved = P.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer

parens :: Parser a -> Parser a
parens = P.parens lexer

dot :: Parser String
dot = P.dot lexer

identifier :: Parser String
identifier = P.identifier lexer
