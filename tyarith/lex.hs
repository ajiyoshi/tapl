module Lex where

import qualified Text.Parsec.Token as P
import Text.Parsec.String
import Text.Parsec.Language

lexer :: P.TokenParser ()
lexer = P.makeTokenParser (javaStyle {
        P.reservedNames = ["if", "then", "else", "true", "false", "succ", "pred", "iszero"]
        } )

reserved :: String -> Parser ()
reserved = P.reserved lexer

natural :: Parser Integer
natural = P.natural lexer

parens :: Parser a -> Parser a
parens = P.parens lexer
