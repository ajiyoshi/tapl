module Core where

import Control.Applicative

data Term = TmTrue | TmFalse | TmNat Integer | TmIf Term Term Term
  | TmSucc Term | TmPred Term | TmIszero Term | TmZero deriving (Show, Read)

data Type = TyNat | TyBool deriving (Show, Read, Eq)

isNumericalVal :: Term -> Bool
isNumericalVal t = case t of
  TmZero -> True
  TmSucc t1 -> isNumericalVal t1
  TmNat _ -> True
  _ -> False

isVal :: Term -> Bool
isVal t = case t of
  TmTrue -> True
  TmFalse -> True
  t0 | isNumericalVal t0 -> True
  _ -> False

eval1 :: Term -> Maybe Term
eval1 t = case t of
  TmIf TmTrue t2 _ -> return t2
  TmIf TmFalse _ t3 -> return t3
  TmIf t1 t2 t3 -> (\t1' -> TmIf t1' t2 t3) <$> eval1 t1
  
  TmSucc (TmNat n) -> return $ TmNat (n+1)
  TmSucc t1 -> TmSucc <$> eval1 t1

  TmPred (TmNat 0) -> return TmZero
  TmPred TmZero -> return TmZero
  TmPred (TmSucc t1) -> return t1
  TmPred t1 -> TmPred <$> eval1 t1

  TmIszero (TmNat 0) -> return TmTrue
  TmIszero TmZero -> return TmTrue
  TmIszero (TmSucc n) | isNumericalVal n -> return TmFalse
  TmIszero t1 -> TmIszero <$> eval1 t1

  _ -> Nothing

eval :: Term -> Term
eval t = case eval1 t of
  Just t1 -> eval t1
  Nothing -> t

typeof :: Term -> Either String Type
typeof t = case t of
  TmTrue -> return TyBool
  TmFalse -> return TyBool

  TmIf t1 t2 t3 -> do
    tyT1 <- typeof t1
    tyT2 <- typeof t2
    tyT3 <- typeof t3
    if tyT1 == TyBool
    then  
      if tyT2 == tyT3
      then return tyT2
      else fail "帰結部と代替部の型が違いました"
    else
      fail "条件部が真偽値ではありませんでした"

  TmZero -> return TyNat
  TmSucc t1 -> do
    tyT1 <- typeof t1
    if tyT1 == TyNat
    then return TyNat
    else fail "succの引数が数値ではありませんでした"
  TmPred t1 -> do
    tyT1 <- typeof t1
    if tyT1 == TyNat
    then return TyNat
    else fail "predの引数が数値ではありませんでした"

  TmIszero t1 -> do
    tyT1 <- typeof t1
    if tyT1 == TyNat
    then return TyBool
    else fail "iszero の引数が数値ではありませんでした"

  TmNat _ -> return TyNat

