module Lambda (
buildIn
, buildInContext
, readLambda
, evalLambda
, calcLambda
, repLambda
, putsMaybe
) where

import Control.Applicative
import Untyped
import UntypedParser

buildIn :: [(String, String)]
buildIn = [
  ("id",    "^x. x")
  ,("tru",  "^t. ^f. t")
  ,("fls",  "^t. ^f. f")
  ,("test", "^l. ^m. ^n. l m n")
  ,("and",  "^b. ^c. b c fls")
  ,("or",   "^b. ^c. b tru c")
  ,("not",  "^b. b fls tru")
  ,("pair", "^f. ^s. ^b. b f s")
  ,("fst",  "^p. p tru")
  ,("snd",  "^p. p fls")
  ,("zero", "^s. ^z. z")
  ,("one",  "^s. ^z. s z")
  ,("two",  "^s. ^z. s (s z)")
  ,("three","^s. ^z. s (s (s z))")
  ,("four", "^s. ^z. s (s (s (s z)))")
  ,("scc",  "^n. ^s. ^z. s (n s z)")
  ,("plus", "^m. ^n. ^s. ^z. m s (n s z)")
  ,("times","^m. ^n. m (plus n) zero")
  ,("prd",  "^m. fst (m (^p. pair (snd p) (plus one (snd p))) (pair zero zero))")
  ,("iszro","^m. m (^x. fls) tru")
  ,("subtract","^m. ^n. n prd m")
  ,("equal","^m. ^n. (and (iszro (n prd m)) (iszro (m prd n)))")
  ,("fix",  "^f. (^x. f (^y. x x y)) (^x. f (^y. x x y))")
  ,("fact", "^f. ^n. test (iszro n) (^x. one) (^x. (times n (f (prd n)))) zero")
  ]

compiledBuildIn :: [(String, Term)]
compiledBuildIn = compile buildIn [] where
  compile :: [(String, String)] -> [(String, Term)] -> [(String, Term)]
  compile [] acc = acc
  compile (p:ps) acc = case parseStrIn [fst x | x <- acc] (snd p) of
    Just t  -> compile ps ((fst p, t) : acc)
    Nothing -> compile ps acc

buildInContext :: [String]
buildInContext = [fst x | x <- compiledBuildIn]

bindBuildIn :: Term -> Term
bindBuildIn x = foldl bind x compiledBuildIn where
  bind :: Term -> (String, Term) -> Term
  bind t p = TmApp (TmAbs (fst p) t) (snd p)

evalLambda :: Term -> Term
evalLambda = eval . bindBuildIn

readLambda :: String -> Maybe Term
readLambda = parseStrIn buildInContext

-- |
-- >>> calcLambda "test tru one two"
-- Just (TmAbs "s" (TmAbs "z" (TmApp (TmVar 1 2) (TmVar 0 2))))
calcLambda :: String -> Maybe Term
calcLambda s = evalLambda <$> readLambda s

-- |
-- >>> repLambda "test tru one two"
-- (^ s. (^ z. (s z)))
--
-- >>> repLambda "and tru fls"
-- (^ t. (^ f. f))
-- >>> repLambda "and fls fls"
-- (^ t. (^ f. f))
-- >>> repLambda "and tru fls"
-- (^ t. (^ f. f))
-- >>> repLambda "and tru tru"
-- (^ t. (^ f. t))
--
-- >>> repLambda "or tru fls"
-- (^ t. (^ f. t))
-- >>> repLambda "or fls fls"
-- (^ t. (^ f. f))
-- >>> repLambda "or tru fls"
-- (^ t. (^ f. t))
-- >>> repLambda "or tru tru"
-- (^ t. (^ f. t))
--
-- >>> repLambda "not tru"
-- (^ t. (^ f. f))
-- >>> repLambda "not fls"
-- (^ t. (^ f. t))
--
-- [残念ながらλ抽象の中の簡約基は簡約されない]
-- >>> repLambda "scc one"
-- (^ s. (^ z. (s (((^ s0. (^ z0. (s0 z0))) s) z))))
--
repLambda :: String -> IO ()
repLambda s0 = putsMaybe $ showTerm <$> calcLambda s0

