import Parser
import Syntax
import Core

import Text.Parsec.String (parseFromFile)
import Control.Applicative ((<$>))

exec :: Context -> [Command] -> IO()
exec _ [] = return ()
exec ctx (cmd:cs) = 
  let
    (ctx', str) = run ctx cmd
  in do
    print str
    exec ctx' cs

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
