module Parser where

import Core
import qualified Lex as L

import Data.List
import Control.Applicative hiding ((<|>))
import Text.Parsec
import Text.Parsec.String

{-
 - Term := AppTerm | LAMBDA LCID COLON Type DOT Term | IF Term THEN Term ELSE Term
 - AppTerm := ATerm | AppTerm ATerm
 - ATerm := LPAREN Term RPAREN | LCID | TRUE | FALSE
 -
 - Type := ArrowType
 - ArrowType := AType ARROW ArrowType | AType
 - AType := LPAREN Type RPAREN | BOOL
 -
 -}

term :: Context -> Parser Term
term ctx = appTerm ctx <|> lambda ctx <|> ifThenElse ctx

appTerm :: Context -> Parser Term
appTerm ctx = aTerm ctx `chainL` ( TmApp <$ L.whiteSpace )

aTerm :: Context -> Parser Term
aTerm ctx = do
    t <- L.parens $ term ctx
    return t
  <|> do
    L.reserved "true"
    return TmTrue
  <|> do
    L.reserved "false"
    return TmFalse
  <|> do
    name <- L.identifier
    case name2index ctx name of
      Just index -> return $ TmVar index (fromIntegral $ length ctx)
      Nothing -> parserFail $ "unknown variable [" ++ name ++ "]"

name2index :: Context -> String -> Maybe Integer
name2index ctx name = fromIntegral <$> elemIndex (name,NameBind) ctx

lambda :: Context -> Parser Term
lambda ctx = do
  L.reservedOp "^" <|> L.reserved "lambda"
  name <- L.identifier
  L.reservedOp ":"
  ty <- typeP ctx
  L.dot
  t <- term (addName ctx name)
  return $ TmAbs name ty t

ifThenElse :: Context -> Parser Term
ifThenElse ctx = do
  L.reserved "if"
  t1 <- term ctx
  L.reserved "then"
  t2 <- term ctx
  L.reserved "else"
  t3 <- term ctx
  return $ TmIf t1 t2 t3

typeP :: Context -> Parser Type
typeP = arrowType

arrowType :: Context -> Parser Type
arrowType ctx = 
  try ( do
    t1 <- aType ctx
    L.reservedOp "->"
    t2 <- typeP ctx
    return $ TyArr t1 t2 )
  <|>
  aType ctx

aType :: Context -> Parser Type
aType ctx =
  do
    t <- L.parens $ typeP ctx
    return t
  <|>
  do
    L.reserved "Bool"
    return TyBool

chainL :: Parser a -> Parser (a -> a -> a) -> Parser a
chainL p op = p >>= op `chain` p

chain :: Parser (a -> a -> a) -> Parser a -> a -> Parser a
chain op p l = ((lefty <$> op <*> p) >>= chain op p) <|> pure l
  where
    lefty f r = l `f` r

runTypeof :: String -> Either String Type
runTypeof str = case parse (term []) "PARSE ERROR" str of
  Left p  -> fail $ show p
  Right s -> typeof [] s

