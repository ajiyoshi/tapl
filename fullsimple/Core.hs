module Core where

import Syntax
import Control.Applicative  ((<$>), (<*>), pure, Applicative)

isval :: Context -> Term -> Bool
isval ctx t = case t of
  TmTrue  -> True
  TmFalse -> True
  TmNat _ -> True
  TmAbs {} -> True
  TmTuple ts -> all (isval ctx) ts
  TmRecord ts -> all (isval ctx) $ map snd ts
  _ -> False

mapFirst :: Applicative f => (a -> Bool) -> (a -> f a) -> [a] -> f [a]
mapFirst _ _ [] = pure []
mapFirst p f (x:xs) =
  if p x
  then (:) <$> pure x <*> mapFirst p f xs
  else (:) <$> f x <*> pure xs

eval1 :: Context -> Term -> Maybe Term
eval1 ctx t = case t of
  TmApp (TmAbs _ _ t1) v2 | isval ctx v2 -> return $ termSubstTop v2 t1
  TmApp v1 t2 | isval ctx v1 -> TmApp v1 <$> eval1 ctx t2
  TmApp t1 t2 -> (`TmApp` t2) <$> eval1 ctx t1

  TmIf TmTrue  t2 _ -> return t2
  TmIf TmFalse _ t3 -> return t3
  TmIf t1 t2 t3 -> (\x -> TmIf x t2 t3) <$> eval1 ctx t1

  TmLet _ v1 t2 | isval ctx v1 -> return ( termSubstTop v1 t2 )
  TmLet s v1 t2 -> (\x -> TmLet s x t2) <$> eval1 ctx v1

  TmProj n t1@(TmTuple vs) | isval ctx t1 -> return $ (undefined:vs) !! fromIntegral n
  TmProj n t1 -> TmProj n <$> eval1 ctx t1

  TmTuple ts | not $ all (isval ctx) ts -> TmTuple <$> mapFirst (isval ctx) (eval1 ctx) ts

  TmProjRec s r@(TmRecord vs) | isval ctx r -> lookup s vs
  TmProjRec s t1 -> TmProjRec s <$> eval1 ctx t1

  TmRecord ts | not $ all isval' ts -> TmRecord <$> mapFirst isval' eval1' ts where
    isval' (_, v) = isval ctx v
    eval1' (k, v) = (\v' -> (k, v')) <$> eval1 ctx v

  TmVar n _ -> case getBinding ctx n of
    (TmAbbBind t1 _) -> return t1
    _ -> Nothing

  _ -> Nothing

eval :: Context -> Term -> Term
eval ctx t = case eval1 ctx t of
  Just t1 -> eval ctx t1
  Nothing -> t

getAbbType :: Context -> Integer -> Maybe Type
getAbbType ctx i = case getBinding ctx i of
  TyAbbBind t -> return t
  _ -> Nothing

evalType :: Context -> Type -> Type
evalType ctx t = case t of
  TyVar x _ -> case getAbbType ctx x of
    Just t' -> t'
    Nothing -> t
  other -> other

eqType :: Context -> Type -> Type -> Bool
eqType ctx t1 t2 =
  (evalType ctx t1) == (evalType ctx t2)

typeof :: Context -> Term -> Either String Type
typeof ctx t = case t of
  TmVar i _ -> getTypeFromContext ctx i
  TmAbs x tyT1 t2 ->
    let ctx' = addBinding ctx x (VarBind tyT1)
    in TyArr tyT1 . typeShift (-1) <$> typeof ctx' t2
  TmApp t1 t2 -> do
      tyT1 <- typeof ctx t1
      tyT2 <- typeof ctx t2
      case evalType ctx tyT1 of
        TyArr tyT11 tyT12 | eqType ctx tyT2 tyT11 -> return tyT12
        TyArr _ _ -> fail "parameter type mismatch"
        x -> fail $ "arrow type expected. but was (" ++ show x ++ ") in " ++ show ctx
  TmTrue  -> return TyBool
  TmFalse -> return TyBool
  TmNat _ -> return TyNat
  TmIf t1 t2 t3 -> do
      t1'  <- typeof ctx t1
      tyT2 <- typeof ctx t2
      tyT3 <- typeof ctx t3
      case (t1', tyT2, tyT3) of
        (TyBool, s2, s3) | s2 == s3 -> return tyT2
        (TyBool, _, _) -> fail "帰結部と代替部の型が違った"
        (_, _, _) -> fail "条件部が真理値でなかった"
  TmLet s v1 t2 -> do
    tyV1 <- typeof ctx v1
    typeShift (-1) <$> typeof (addBinding ctx s (VarBind tyV1)) t2
  TmTuple vs -> TyTuple <$> eitherAll (typeof ctx) vs
  TmProj n t1 -> do
    tyT1 <- typeof ctx t1
    case tyT1 of
      TyTuple ts  -> return $ (undefined:ts) !! fromIntegral n
      _ -> fail "ペアではないものから中身を取ろうとした"
  TmRecord ts -> TyRecord . zip keys <$> eitherAll (typeof ctx) vals where
    keys = map fst ts
    vals = map snd ts
  TmProjRec s t1 -> do
    tyT1 <- typeof ctx t1
    case tyT1 of
      TyRecord ts -> case lookup s ts of
        Just ty -> return ty
        Nothing -> fail $ "存在しないレコードキー:" ++ s
      _ -> fail "レコードでないものから中身を取ろうとした"

eitherAll :: (a -> Either b c) -> [a] -> Either b [c]
eitherAll _ [] = return []
eitherAll f (x:xs) = (:) <$> f x <*> eitherAll f xs

