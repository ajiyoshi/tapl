module Core where

data Type = TyArr Type Type | TyBool deriving (Show, Read, Eq)

data Binding = NameBind | VarBind Type deriving (Show, Read, Eq)

data Term = TmVar Integer Integer
  | TmAbs String Type Term
  | TmApp Term Term
  | TmTrue
  | TmFalse
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
isval _ t = case t of
  TmTrue -> True
  TmFalse -> True
  TmAbs _ _ _ -> True
  _ -> False

eval1 :: Context -> Term -> Maybe Term
eval1 ctx t = case t of
  TmApp (TmAbs _ _ t1) v2 | isval ctx v2 ->
    Just ( termSubstTop v2 t1 )
  TmApp v1 t2 | isval ctx v1 -> do
    t2' <- eval1 ctx t2
    return (TmApp v1 t2')
  TmApp t1 t2 -> do
    t1' <- eval1 ctx t1
    return (TmApp t1' t2)
  TmIf TmTrue t2 _ -> Just t2
  TmIf TmFalse _ t3 -> Just t3
  TmIf t1 t2 t3 -> do
    t1' <- eval1 ctx t1
    return (TmIf t1' t2 t3)
  _ -> Nothing

tmmap :: (Integer -> Integer -> Integer -> Term) -> Integer -> Term -> Term
tmmap onvar c0 t0 = 
  let
    walk c t = case t of
      TmVar x n -> onvar c x n
      TmAbs x tyT1 t2 -> TmAbs x tyT1 (walk (c+1) t2)
      TmApp t1 t2 -> TmApp (walk c t1) (walk c t2)
      TmTrue -> TmTrue
      TmFalse -> TmFalse
      TmIf t1 t2 t3 -> TmIf (walk c t1) (walk c t2) (walk c t3)
  in walk c0 t0

termShiftAbove :: Integer -> Integer -> Term -> Term
termShiftAbove d c0 t0 =
  tmmap
    (\c x n -> if x >= c then TmVar (x+d) (n+d) else TmVar x (n+d))
    c0 t0

termShift :: Integer -> Term -> Term
termShift d t = termShiftAbove d 0 t

termSubst :: Integer -> Term -> Term -> Term
termSubst j0 s0 t0 =
  tmmap
    (\j x n -> if x == j then termShift j s0 else TmVar x n)
    j0 t0

termSubstTop :: Term -> Term -> Term
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)

typeof :: Context -> Term -> Either String Type
typeof ctx t = case t of
  TmVar i _ -> getTypeFromContext ctx i
  TmAbs x tyT1 t2 ->
    let ctx' = addBinding ctx x (VarBind tyT1)
    in do
        t2' <- typeof ctx' t2
        return $ TyArr tyT1 t2'
  TmApp t1 t2 -> do
      tyT1 <- typeof ctx t1
      tyT2 <- typeof ctx t2
      case tyT1 of
        TyArr tyT11 tyT12 | tyT2 == tyT11 -> return tyT12
        TyArr tyT11 _     | tyT2 /= tyT11 -> fail "parameter type mismatch"
        _ -> fail "arrow type expected"
  TmTrue  -> return TyBool
  TmFalse -> return TyBool
  TmIf t1 t2 t3 -> do
      t1'  <- typeof ctx t1
      tyT2 <- typeof ctx t2
      tyT3 <- typeof ctx t3
      if t1' == TyBool
      then
        if tyT2 == tyT3
        then return tyT2
        else fail "arms of conditional have different types"
      else fail "guard of conditional not a boolean"


