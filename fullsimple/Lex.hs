module Lex where

import Text.Parsec ((<|>), letter, char, eof, parseTest)
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import Text.Parsec.String (Parser)

lexer :: P.TokenParser ()
lexer = P.makeTokenParser ( haskellDef {
       P.reservedNames = ["if", "then", "else", "true", "false", "Bool", "lambda", "Nat", "unit", "Unit",
                         "let", "in"]
       , P.reservedOpNames = ["^", "->", "_"]
       , P.identStart = letter <|> char '_'
       } )

reserved :: String -> Parser ()
reserved = P.reserved lexer

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

parens :: Parser a -> Parser a
parens = P.parens lexer

braces :: Parser a -> Parser a
braces = P.braces lexer

identifier :: Parser String
identifier = P.identifier lexer

reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer

natural :: Parser Integer
natural = P.natural lexer

dot :: Parser String
dot = P.dot lexer

comma :: Parser String
comma = P.comma lexer

commaSep :: Parser a -> Parser [a]
commaSep = P.commaSep lexer

colon :: Parser String
colon = P.colon lexer

semi :: Parser String
semi = P.semi lexer

runLex :: Show a => Parser a -> String -> IO()
runLex p = parseTest ( do { 
                          whiteSpace
                          ; x <- p
                          ; eof
                          ; return x
                          }) 

