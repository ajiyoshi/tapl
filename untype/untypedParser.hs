module UntypeParser (
term
) where

import Untyped
import Data.List
import Control.Applicative hiding ((<|>))
import Text.Parsec
import Text.Parsec.String

type Context = [String] 

term :: Context -> Parser Term
term ctx = appTerm ctx <|> lambda ctx

appTerm :: Context -> Parser Term
appTerm ctx = atomicTerm ctx `chainL` ( TmApp <$ spaces )

atomicTerm :: Context -> Parser Term
atomicTerm ctx = do
  char '('
  t <- term ctx
  char ')'
  return t
  <|> identifier ctx

identifier :: Context -> Parser Term
identifier ctx = do
  name <- many1 letter
  case name2index ctx name of
    Just index -> return $ TmVar name index (fromIntegral $ length ctx)
    Nothing -> parserFail $ "unknown variable [" ++ name ++ "]"

lambda :: Context -> Parser Term
lambda ctx = do
  char '^'
  spaces
  name <- many1 letter
  spaces
  char '.'
  spaces
  t <- term (name:ctx)
  return $ Lambda t

name2index :: Context -> String -> Maybe Integer
name2index ctx name = do
  n <- elemIndex name ctx
  return $ fromIntegral n

chainL :: Parser a -> Parser (a -> a -> a) -> Parser a
chainL p op = p >>= op `chain` p

chain :: Parser (a -> a -> a) -> Parser a -> a -> Parser a
chain op p l = ((lefty <$> op <*> p) >>= chain op p) <|> pure l
  where
    lefty f r = l `f` r
