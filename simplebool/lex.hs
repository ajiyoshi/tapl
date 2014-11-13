module Lex where

import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language 
import Text.Parsec.String

lexer :: P.TokenParser ()
lexer = P.makeTokenParser ( haskellDef {
       P.reservedNames = ["if", "then", "else", "true", "false", "Bool", "lambda"]
       , P.reservedOpNames = ["^", "->", ":"]
       } )

reserved :: String -> Parser ()
reserved = P.reserved lexer

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

parens :: Parser a -> Parser a
parens = P.parens lexer

identifier :: Parser String
identifier = P.identifier lexer

reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer

dot :: Parser String
dot = P.dot lexer

runLex :: Show a => Parser a -> String -> IO()
runLex p = parseTest ( do { 
                          whiteSpace
                          ; x <- p
                          ; eof
                          ; return x
                          }) 

