module UntypedParser (
Context
, term
, pickfreshname
, showTerm
, readTerm
, calcTerm
, repTerm

, putsMaybe
, parseStrIn
) where

import Untyped
import Data.List
import Control.Applicative hiding ((<|>))
import Text.Parsec
import Text.Parsec.String

type Context = [String] 

termToString :: Context -> Term -> String
termToString ctx t = case t of
  TmAbs name t1 ->
    let (ctx', x') = pickfreshname ctx name
    in "(^ " ++ x' ++ ". " ++ termToString ctx' t1 ++ ")"
  TmApp t1 t2 ->
    "(" ++ termToString ctx t1 ++ " " ++ termToString ctx t2 ++ ")"
  TmVar x n 
    | length ctx == fromIntegral n -> ctx !! fromIntegral x
    | otherwise -> "[bad index]"

pickfreshname :: Context -> String -> (Context, String)
pickfreshname ctx name = choose names where
  choose (n:ns) = case elemIndex n ctx of
    Just _ -> choose ns
    Nothing -> ( n:ctx, n )
  names = name : [ name ++ (show::Integer->String) i | i <- [0..] ]
  
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
  return $ TmAbs name t

name2index :: Context -> String -> Maybe Integer
name2index ctx name = fromIntegral <$> elemIndex name ctx

chainL :: Parser a -> Parser (a -> a -> a) -> Parser a
chainL p op = p >>= op `chain` p

chain :: Parser (a -> a -> a) -> Parser a -> a -> Parser a
chain op p l = ((lefty <$> op <*> p) >>= chain op p) <|> pure l
  where
    lefty f r = l `f` r

parseStrIn :: Context -> String -> Maybe Term
parseStrIn ns str = case parse (term ns) "error" str of
  Left _ -> Nothing
  Right t -> Just t

readTerm :: String -> Maybe Term
readTerm s = parseStrIn [] s

-- |
-- >>> calcTerm "^x.x"
-- Just (TmAbs "x" (TmVar 0 1))
--
-- >>> calcTerm "^x.y"
-- Nothing
calcTerm :: String -> Maybe Term
calcTerm s = eval <$> readTerm s

-- |
-- >>> repTerm "^x.x"
-- (^ x. x)
--
-- >>> repTerm "^x.y"
-- ERROR
--
-- [左結合]
-- >>> repTerm "^x. x x x"
-- (^ x. ((x x) x))
--
-- [カッコ]
-- >>> repTerm "^x. x (x x)"
-- (^ x. (x (x x)))
--
-- [簡約]
-- >>> repTerm "(^x. x (x x))(^y. y)"
-- (^ y. y)
--
-- [簡約]
-- >>> repTerm "(^y. y)(^x. x (x x))"
-- (^ x. (x (x x)))
repTerm :: String -> IO ()
repTerm s = putsMaybe $ showTerm <$> calcTerm s
 
showTerm :: Term -> String
showTerm = termToString []

putsMaybe :: Maybe String -> IO()
putsMaybe (Just s) = putStrLn s
putsMaybe Nothing = putStrLn "ERROR"
