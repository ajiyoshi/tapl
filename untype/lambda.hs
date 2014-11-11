module Lambda where

import Control.Applicative
import Untyped
import UntypedParser

buildIn :: [(String, String)]
buildIn = [
  ("id", "^x. x")
  ,("tru", "^t. ^f. t")
  ,("fls", "^t. ^f. f")
  ,("test", "^l. ^m. ^n. l m n")
  ,("and", "^b. ^c. b c fls")
  ,("pair", "^f. ^s. ^b. b f s")
  ,("fst", "^p. p tru")
  ,("snd", "^p. p fls")
  ,("zero", "^s. ^z. z")
  ,("one", "^s. ^z. s z")
  ,("two", "^s. ^z. s (s z)")
  ,("three", "^s. ^z. s (s (s z))")
  ,("four", "^s. ^z. s (s (s (s z)))")
  ,("scc", "^n. ^s. ^z. s (n s z)")
  ,("plus", "^m. ^n. ^s. ^z. m s (n s z)")
  ,("times", "^m. ^n. m (plus n) zero")
  ,("prd", "^m. fst (m (^p. pair (snd p) (plus one (snd p))) (pair zero zero))")
  ,("iszro", "^m. m (^x. fls) tru")
  ,("subtract", "^m. ^n. n prd m")
  ,("equal", "^m. ^n. (and (iszro (n prd m)) (iszro (m prd n)))")
  ,("fix", "^f. (^x. f (^y. x x y)) (^x. f (^y. x x y))")
  ,("fact", "^f. ^n. test (iszro n) (^x. one) (^x. (times n (f (prd n)))) zero")
  ]

compile :: [(String, String)] -> [(String, Term)] -> [(String, Term)]
compile [] acc = acc
compile (p:ps) acc = case parseStrIn [fst x | x <- acc] (snd p) of
  Just t -> compile ps ( (fst p, t) : acc)
  Nothing -> compile ps acc

compiledBuildIn :: [(String, Term)]
compiledBuildIn = compile buildIn []

buildInContext :: [String]
buildInContext = [ fst x | x <- compiledBuildIn ]

bindBuildIn :: Term -> Term
bindBuildIn x = foldl bind x compiledBuildIn where
  bind :: Term -> (String, Term) -> Term
  bind t p = TmApp (TmAbs (fst p) t) (snd p)

evalLambda :: Term -> Term
evalLambda = eval . bindBuildIn

readLambda :: String -> Maybe Term
readLambda = parseStrIn buildInContext

calcLambda :: String -> Maybe Term
calcLambda s = evalLambda <$> readLambda s

repLambda :: String -> IO ()
repLambda s0 = putsMaybe $ showTerm <$> calcLambda s0

putsMaybe :: Maybe String -> IO()
putsMaybe (Just s) = putStrLn s
putsMaybe Nothing = return ()
