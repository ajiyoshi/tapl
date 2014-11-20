import Parser
import Syntax
import Core

import Text.Parsec.String (parseFromFile)
import Control.Applicative ((<$>))

exec :: Context -> [Command] -> IO()
exec _ [] = return ()
exec ctx (c:cs) = case c of
  Eval t1 -> case typeof ctx t1 of
    Left s -> do
      print s
    Right ty -> do
      print $ show (eval ctx t1) ++ " : " ++ show ty
      exec ctx cs
  Bind n b0 ->
    case checkBiding ctx b0 of
      Left s -> do
        print s
      Right b1 ->
        let b2 = evalBinding ctx b1 in
        do
          print $ n ++ " : " ++ show b2
          exec (addBinding ctx n b2) cs

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
