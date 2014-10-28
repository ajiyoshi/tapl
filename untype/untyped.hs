module Untyped (
Term
, termShift
, termSubst
, termSubstTop
, isval
, eval1
, eval
) where

data Term = TmVar Integer Integer | Lambda Term | TmApp Term Term deriving ( Show, Read )

--type Context = [(String, String)] 

termShift :: Integer -> Term -> Term
termShift d term =
  let
    walk c t = case t of
      TmVar x n
        | x >= c -> TmVar (x+d) (n+d)
        | otherwise -> TmVar x (n+d)
      Lambda t1 -> Lambda $ walk (c+1) t1
      TmApp t1 t2 -> TmApp (walk c t1) (walk c t2)
  in walk 0 term
    
termSubst :: Integer -> Term -> Term -> Term
termSubst j s term =
  let
    walk c t = case t of
      TmVar x n
        | x == j + c -> termShift c s
        | otherwise -> TmVar x n
      Lambda t1 -> Lambda (walk (c+1) t1)
      TmApp t1 t2 -> TmApp (walk c t1) (walk c t2)
  in walk 0 term

termSubstTop :: Term -> Term -> Term
termSubstTop s t =
  termShift (-1) (termSubst 0 (termShift 1 s) t)

isval :: Term -> Bool
isval term = case term of
    Lambda _ -> True
    _ -> False 

eval1 :: Term -> Maybe Term
eval1 term = case term of
  TmApp (Lambda t12) v2
    | isval v2 -> Just ( termSubstTop v2 t12 )
  TmApp v1 t2 | isval v1 -> do
    t2' <- eval1 t2
    return ( TmApp v1 t2' )
  TmApp t1 t2 -> do
    t1' <- eval1 t1
    return ( TmApp t1' t2 )
  _ -> Nothing

eval :: Term -> Term
eval term =
  case eval1 term of
    Just t -> eval t
    Nothing -> term 
