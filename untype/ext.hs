module Ext (
ExtTerm(..)
, evalExt
, evalNExt
, calcExt
, repExt
, realnat
, realbool
, showExtTerm
) where

import Control.Applicative
import Core
import Parser
import Lambda

data ExtTerm = Var Integer Integer | Lambda String ExtTerm | Apply ExtTerm ExtTerm
  | B Bool | N Integer | Succ deriving (Show)

termShiftExt :: Integer -> ExtTerm -> ExtTerm
termShiftExt d tm =
  let
    walk c t = case t of
      Var x n
        | x >= c -> Var (x+d) (n+d)
        | otherwise -> Var x (n+d)
      Lambda name t1 -> Lambda name (walk (c+1) t1)
      Apply t1 t2 -> Apply (walk c t1) (walk c t2)
      B b -> B b
      N n -> N n
      Succ -> Succ
  in walk 0 tm
    
termSubstExt :: Integer -> ExtTerm -> ExtTerm -> ExtTerm
termSubstExt j s tm =
  let
    walk c t = case t of
      Var x n
        | x == j + c -> termShiftExt c s
        | otherwise -> Var x n
      Lambda name t1 -> Lambda name (walk (c+1) t1)
      Apply t1 t2 -> Apply (walk c t1) (walk c t2)
      B b -> B b
      N n -> N n
      Succ -> Succ
  in walk 0 tm

termSubstTopExt :: ExtTerm -> ExtTerm -> ExtTerm
termSubstTopExt s t =
  termShiftExt (-1) (termSubstExt 0 (termShiftExt 1 s) t)

isvalExt :: ExtTerm -> Bool
isvalExt t0 = case t0 of
    Lambda _ _ -> True
    B _ -> True
    N _ -> True
    Succ -> True
    _ -> False 

eval1Ext :: ExtTerm -> Maybe ExtTerm
eval1Ext t0 = case t0 of
  Apply Succ (N n) -> Just $ N (n+1)
  Apply Succ t2 -> do
    t2' <- eval1Ext t2
    return ( Apply Succ t2' )
  Apply (Lambda _ t12) v2 | isvalExt v2 ->
    Just ( termSubstTopExt v2 t12 )
  Apply v1 t2 | isvalExt v1 -> do
    t2' <- eval1Ext t2
    return ( Apply v1 t2' )
  Apply t1 t2 -> do
    t1' <- eval1Ext t1
    return ( Apply t1' t2 )
  _ -> Nothing

evalNExt :: Integer -> ExtTerm -> ExtTerm
evalNExt 0 t = t
evalNExt n t = case eval1Ext t of
  Just t1 -> evalNExt (n-1) t1
  Nothing -> t
  

evalExt :: ExtTerm -> ExtTerm
evalExt t0 =
  case eval1Ext t0 of
    Just t1 -> evalExt t1
    Nothing -> t0 

extFromTerm :: Term -> ExtTerm
extFromTerm t0 = case t0 of
  TmVar n1 n2 -> Var n1 n2
  TmAbs name t1 -> Lambda name $ extFromTerm t1
  TmApp t1 t2 -> Apply (extFromTerm t1) (extFromTerm t2)

-- |
-- >>> calcExt "one"
-- Just (Lambda "s" (Lambda "z" (Apply (Var 1 2) (Var 0 2))))
calcExt :: String -> Maybe ExtTerm
calcExt s = extFromTerm <$> calcLambda s

repExt :: String -> IO ()
repExt s = putsMaybe $ showExtTerm <$> calcExt s

-- |
-- >>> realbool <$> calcExt "tru"
-- Just (B True)
--
-- >>> realbool <$> calcExt "fls"
-- Just (B False)
--
-- >>> realbool <$> calcExt "equal one one"
-- Just (B True)
-- 
-- >>> realbool <$> calcExt "equal one two"
-- Just (B False)
--
-- >>> realbool <$> calcExt "and tru fls"
-- Just (B False)
-- >>> realbool <$> calcExt "and fls fls"
-- Just (B False)
-- >>> realbool <$> calcExt "and fls tru"
-- Just (B False)
-- >>> realbool <$> calcExt "and tru tru"
-- Just (B True)
--
-- >>> realbool <$> calcExt "or tru fls"
-- Just (B True)
-- >>> realbool <$> calcExt "or fls fls"
-- Just (B False)
-- >>> realbool <$> calcExt "or tru fls"
-- Just (B True)
-- >>> realbool <$> calcExt "or tru tru"
-- Just (B True)
--
-- >>> realbool <$> calcExt "not tru"
-- Just (B False)
-- >>> realbool <$> calcExt "not fls"
-- Just (B True)
--
-- >>> realbool <$> calcExt "equal three (prd (times (scc one) two))"
-- Just (B True)
-- >>> realbool <$> calcExt "equal four (prd (times (scc one) two))"
-- Just (B False)
--
-- >>> realbool <$> calcExt "iszro zero"
-- Just (B True)
-- >>> realbool <$> calcExt "iszro one"
-- Just (B False)
-- >>> realbool <$> calcExt "iszro (subtract four (times (scc one) two))"
-- Just (B True)
-- >>> realbool <$> calcExt "iszro (subtract four (prd (times (scc one) two)))"
-- Just (B False)
realbool :: ExtTerm -> ExtTerm
realbool t = evalExt $ Apply (Apply t (B True)) (B False)

-- |
-- >>> realnat <$> calcExt "one"
-- Just (N 1)
--
-- >>> realnat <$> calcExt "fix fact three"
-- Just (N 6)
--
-- >>> realnat <$> calcExt "fix fact three"
-- Just (N 6)
--
-- >>> realnat <$> calcExt "subtract four three"
-- Just (N 1)
--
-- >>> realnat <$> calcExt "prd (times (scc one) two)"
-- Just (N 3)
--
-- >>> realnat <$> calcExt "subtract four (times (scc one) two)"
-- Just (N 0)
realnat :: ExtTerm -> ExtTerm
realnat t = evalExt $ Apply (Apply t Succ) (N 0)

showExtTerm :: ExtTerm -> String
showExtTerm = extTermToString []

pad :: Context -> String
pad c = replicate (length c) ' '

extTermToString :: Context -> ExtTerm -> String
extTermToString ctx t = case t of
  Lambda name t1 ->
    let (ctx', x') = pickfreshname ctx name
    in '\n' : pad ctx ++ "(^ " ++ x' ++ ". " ++ extTermToString ctx' t1 ++ ")"
  Apply t1 t2 ->
    "(" ++ extTermToString ctx t1 ++ " " ++ extTermToString ctx t2 ++ ")"
  Var x n 
    | length ctx == fromIntegral n -> ctx !! fromIntegral x
    | otherwise -> "[bad index]"
  B True -> " #t "
  B False -> " #f "
  N n -> show n
  Succ -> "Succ"

