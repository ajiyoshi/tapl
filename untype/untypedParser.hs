module UntypeParser (
term
, Context
, parseStr
, showTerm
, evalStr
, replTerm
) where

import Untyped
import Data.List
import Control.Applicative hiding ((<|>))
import Text.Parsec
import Text.Parsec.String

type Context = [String] 

evalStr :: String -> Maybe Term
evalStr str = do
  t <- parseStr str
  return $ eval t

replTerm :: String -> Maybe String
replTerm s = do
  t <- parseStr s
  return $ (showTerm . eval) t
 
showTerm :: Term -> String
showTerm = termToString []

termToString :: Context -> Term -> String
termToString ctx t = case t of
  Lambda name t1 ->
    let (ctx', x') = pickfreshname ctx name
    in "(^" ++ x' ++ ". " ++ termToString ctx' t1 ++ ")"
  TmApp t1 t2 ->
    "(" ++ termToString ctx t1 ++ " " ++ termToString ctx t2 ++ ")"
  TmVar x n 
    | length ctx == fromIntegral n -> ctx !! fromIntegral x
    | otherwise -> "[bad index]"

pickfreshname :: Context -> String -> (Context, String)
pickfreshname ctx name = case elemIndex name ctx of
  Just _ -> pickfreshname ctx $ name ++ "'"
  Nothing -> ( name:ctx, name )
  
parseStr :: String -> Maybe Term
parseStr str = case parse (term []) "error" str of
  Left _ -> Nothing
  Right t -> Just t

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
    Just index -> return $ TmVar index (fromIntegral $ length ctx)
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
  return $ Lambda name t

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
