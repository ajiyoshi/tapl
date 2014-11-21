import Parser
import Syntax
import Core

import Data.List (foldl')
import Text.Parsec.String (parseFromFile)
import Control.Applicative ((<$>), (<*>))

exec :: Context -> [Command] -> IO()
exec _ [] = return ()
exec ctx (cmd:cs) = 
  let
    (ctx', str) = run ctx cmd
  in do
    putStrLn str
    exec ctx' cs

loadAndRun :: Maybe Context -> String -> Maybe Term
loadAndRun c str = do
  ctx <- c
  case readTerm ctx str of
    Left _ -> Nothing
    Right t -> Just $ eval ctx t

loadAndRun2 :: String -> String -> IO (Maybe Term)
loadAndRun2 file str = loadAndRun <$> load file <*> return str

evalN :: Maybe Context -> Term -> Integer -> Maybe Term
evalN c t 0 = return t
evalN c t n = do
  ctx <- c
  do
    t' <- eval1 ctx t
    evalN c t' (n-1)

load :: String -> IO ( Maybe Context )
load file = do
  hoge <- parseFromFile (toplevel []) file
  return $ case hoge of
    Left x -> Nothing
    Right (cmds, _) -> Just $ foldl' f [] cmds where
      f ctx cmd = let (ctx', _) = run ctx cmd in ctx'

run :: Context -> Command -> (Context, String)
run ctx (Eval t1) = 
    either
      (\s -> (ctx, s))
      (\t -> (ctx, show (eval ctx t1) ++ " : " ++ show t))
      $ typeof ctx t1
run ctx (Bind n b0) =
    either
      (\ s -> (ctx, s))
      (\ b1 ->
        let
          b2 = evalBinding ctx b1
          ret = n ++ " : " ++ show b2
          ctx' = addBinding ctx n b2
        in (ctx', ret) )
      $ checkBiding ctx b0

checkBiding :: Context -> Binding -> Either String Binding
checkBiding ctx b = case b of
  TmAbbBind tm Nothing -> TmAbbBind tm . Just <$> typeof ctx tm
  TmAbbBind tm (Just ty1) -> do
    ty2 <- typeof ctx tm 
    if eqType ctx ty1 ty2
    then return $ TmAbbBind tm (Just ty1)
    else fail $ "型が不一致 (" ++ show ty1 ++ " <=> " ++ show ty2 ++ ")"
  other -> return other
  
evalBinding :: Context -> Binding -> Binding
evalBinding ctx (TmAbbBind t ty) = TmAbbBind (eval ctx t) ty
evalBinding _ b = b

executeFile :: String -> IO()
executeFile file = do
  hoge <- parseFromFile (toplevel []) file
  case hoge of 
    Left x -> print x
    Right (cmd, _) -> exec [] cmd

main :: IO ()
main = executeFile "hoge.ml"
