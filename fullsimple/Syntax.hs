module Syntax where

import Data.List (elemIndex)
import Control.Applicative ((<$>))
import Control.Arrow ((&&&))

data Type =
  TyArr Type Type
  | TyBool
  | TyNat
  | TyUnit
  | TyVar Integer Integer
  | TyTuple [Type]
  | TyVariant [(String, Type)]
  | TyRecord [(String, Type)] deriving (Show, Read, Eq)

data Binding =
  NameBind
  | VarBind Type 
  | TyVarBind
  -- abbreviation?
  | TmAbbBind Term (Maybe Type) 
  | TyAbbBind Type deriving (Show, Read, Eq)

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

data Command = Eval Term | Bind String Binding deriving (Show, Read)

type Context = [(String, Binding)]

addBinding :: Context -> String -> Binding -> Context
addBinding ctx x bind = (x, bind) : ctx

addName :: Context -> String -> Context
addName ctx name = (name, NameBind) : ctx

pickfreshname :: Context -> String -> (Context, String)
pickfreshname ctx "_" = (("_", NameBind):ctx, "_")
pickfreshname ctx name = choose names where
  choose (n:ns) = case elemIndex n $ map fst ctx of
    Just _ -> choose ns
    Nothing -> ( (n, NameBind):ctx, n )
  names = name : [ name ++ (show::Integer->String) i | i <- [0..] ]

name2index :: Context -> String -> Maybe Integer
name2index ctx name = fromIntegral <$> (elemIndex name $ map fst ctx)

tymap :: (Integer -> Integer -> Integer -> Type) -> Integer -> Type -> Type
tymap onvar c0 t0 =
  let
    walk c t = case t of
      TyVar x n -> onvar c x n
      TyArr t1 t2 -> TyArr (walk c t1) (walk c t2)
      TyTuple ts -> TyTuple $ map (walk c) ts
      TyVariant ts -> TyVariant $ map (fst &&& walk c . snd) ts
      TyRecord ts -> TyRecord $ map (fst &&& walk c . snd) ts
      other -> other
  in walk c0 t0

tmmap ::
  (Integer -> Integer -> Integer -> Term) ->
  (Integer -> Type -> Type) ->
  Integer -> Term -> Term
tmmap onvar ontype c0 t0 = 
  let
    walk c t = case t of
      TmVar x n -> onvar c x n
      TmAbs x tyT1 t2 -> TmAbs x (ontype c tyT1) (walk (c+1) t2)
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
    (typeShiftAbobe d)

typeShiftAbobe :: Integer -> Integer -> Type -> Type
typeShiftAbobe d = 
  tymap
    (\c x n -> if x >= c then TyVar (x+d) (n+d) else TyVar x (n+d))

termShift :: Integer -> Term -> Term
termShift d = termShiftAbove d 0

typeShift :: Integer -> Type -> Type
typeShift d = typeShiftAbobe d 0

bindingShift :: Integer -> Binding -> Binding
bindingShift d b = case b of
  NameBind -> NameBind
  VarBind t -> VarBind (typeShift d t)
  TmAbbBind tm (Just t) -> TmAbbBind tm (Just (typeShift d t))
  TmAbbBind tm Nothing -> TmAbbBind tm Nothing
  TyAbbBind t -> TyAbbBind (typeShift d t)
  TyVarBind -> TyVarBind


-- subst

termSubst :: Integer -> Term -> Term -> Term
termSubst j0 s0 =
  tmmap
    (\j x n -> if x == j then termShift j s0 else TmVar x n)
    (\_ ty -> ty)
    j0

typeSubst :: Integer -> Type -> Type -> Type
typeSubst j0 s0 =
  tymap
    (\j x n -> if x == j then typeShift j s0 else TyVar x n)
    j0

termSubstTop :: Term -> Term -> Term
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)

typeSubstTop :: Type -> Type -> Type
typeSubstTop s t = typeShift (-1) (typeSubst 0 (typeShift 1 s) t)

getBinding :: Context -> Integer -> Binding
getBinding ctx i =
  let (_, bin) = ctx !! fromIntegral i
  in bindingShift (i+1) bin

getTypeFromContext :: Context -> Integer -> Either String Type
getTypeFromContext ctx i = case getBinding ctx i of
  VarBind tyT -> return tyT
  TmAbbBind _ (Just tyT) -> return tyT
  TmAbbBind _ Nothing ->  fail "no type recorded for variable" 
  TyAbbBind tyT -> return tyT
  _ -> fail $ "type error : (" ++ show ctx ++ ") " ++ show i

