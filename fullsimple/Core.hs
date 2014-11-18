module Core where

import Control.Applicative  ((<$>), (<*>), pure, Applicative)
import Control.Arrow ((&&&))

data Type = TyArr Type Type
  | TyBool
  | TyNat
  | TyUnit
  | TyTuple [Type]
  | TyVariant [(String, Type)]
  | TyRecord [(String, Type)] deriving (Show, Read, Eq)

data Binding = NameBind | VarBind Type deriving (Show, Read, Eq)

data Term = TmVar Integer Integer
  | TmAbs String Type Term
  | TmApp Term Term
  | TmTrue
  | TmFalse
  | TmNat Integer
  | TmUnit
  | TmLet String Term Term
  | TmTuple [Term]
  | TmProj Integer Term
  | TmRecord [(String, Term)]
  | TmProjRec String Term
  | TmVariant String [(String, Type)]
  | TmMatch Term [(String, String, Term)]
  | TmIf Term Term Term deriving (Show, Read, Eq)

type Context = [(String, Binding)]

addBinding :: Context -> String -> Binding -> Context
addBinding ctx x bind = (x, bind) : ctx

addName :: Context -> String -> Context
addName ctx name = (name, NameBind) : ctx

getTypeFromContext :: Context -> Integer -> Either String Type
getTypeFromContext ctx i = case getBinding ctx i of
  VarBind tyT -> return tyT
  _ -> fail $ "type error : (" ++ show ctx ++ ")"

getBinding :: Context -> Integer -> Binding
getBinding ctx i = let (_, bin) = ctx !! fromIntegral i in bin

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

  _ -> Nothing

eval :: Context -> Term -> Term
eval ctx t = case eval1 ctx t of
  Just t1 -> eval ctx t1
  Nothing -> t

tmmap :: (Integer -> Integer -> Integer -> Term) -> Integer -> Term -> Term
tmmap onvar c0 t0 = 
  let
    walk c t = case t of
      TmVar x n -> onvar c x n
      TmAbs x tyT1 t2 -> TmAbs x tyT1 (walk (c+1) t2)
      TmApp t1 t2 -> TmApp (walk c t1) (walk c t2)
      TmTrue  -> TmTrue
      TmFalse -> TmFalse
      TmNat n -> TmNat n
      TmIf t1 t2 t3 -> TmIf (walk c t1) (walk c t2) (walk c t3)
      TmLet x v1 t2 -> TmLet x (walk c v1) (walk (c+1) t2)
      TmTuple ts -> TmTuple $ map (walk c) ts
      TmProj n t1 -> TmProj n (walk c t1)
      TmRecord ts -> TmRecord (map (fst &&& walk c . snd) ts)
      TmProjRec s t1 -> TmProjRec s (walk c t1)
  in walk c0 t0

termShiftAbove :: Integer -> Integer -> Term -> Term
termShiftAbove d =
  tmmap
    (\c x n -> if x >= c then TmVar (x+d) (n+d) else TmVar x (n+d))

termShift :: Integer -> Term -> Term
termShift d = termShiftAbove d 0

termSubst :: Integer -> Term -> Term -> Term
termSubst j0 s0 =
  tmmap
    (\j x n -> if x == j then termShift j s0 else TmVar x n)
    j0

termSubstTop :: Term -> Term -> Term
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)

eqType :: Either String Type -> Either String Type -> Either String Bool
eqType tyT1 tyT2 = (==) <$> tyT1 <*> tyT2

typeof :: Context -> Term -> Either String Type
typeof ctx t = case t of
  TmVar i _ -> getTypeFromContext ctx i
  TmAbs x tyT1 t2 ->
    let ctx' = addBinding ctx x (VarBind tyT1)
    in TyArr tyT1 <$> typeof ctx' t2
  TmApp t1 t2 -> do
      tyT1 <- typeof ctx t1
      tyT2 <- typeof ctx t2
      case tyT1 of
        TyArr tyT11 tyT12 | tyT2 == tyT11 -> return tyT12
        TyArr _ _ -> fail "parameter type mismatch"
        _ -> fail "arrow type expected"
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
    typeof (addBinding ctx s (VarBind tyV1)) t2
  TmTuple vs -> TyTuple <$> hoge2 (typeof ctx) vs
  TmProj n t1 -> do
    tyT1 <- typeof ctx t1
    case tyT1 of
      TyTuple ts  -> return $ (undefined:ts) !! fromIntegral n
      _ -> fail "ペアではないものから中身を取ろうとした"
  TmRecord ts -> TyRecord . zip keys <$> hoge2 (typeof ctx) vals where
    keys = map fst ts
    vals = map snd ts
  TmProjRec s t1 -> do
    tyT1 <- typeof ctx t1
    case tyT1 of
      TyRecord ts -> case lookup s ts of
        Just ty -> return ty
        Nothing -> fail $ "存在しないレコードキー:" ++ s
      _ -> fail "レコードでないものから中身を取ろうとした"

hoge2 :: (a -> Either b c) -> [a] -> Either b [c]
hoge2 _ [] = return []
hoge2 f (x:xs) = (:) <$> f x <*> hoge2 f xs

      
