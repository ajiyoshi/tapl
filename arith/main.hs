
data Term = TmTrue | TmFalse | TmZero
    | TmSucc Term | TmPred Term | TmIsZero Term
    | TmIf Term Term Term
    deriving (Show, Read)

isNumericVal :: Term -> Bool
isNumericVal term = case term of
    TmZero -> True
    TmSucc t1 -> isNumericVal t1
    _ -> False

eval1 :: Term -> Maybe Term
eval1 term = case term of 
    TmIf TmTrue  t2 t3 -> Just t2
    TmIf TmFalse t2 t3 -> Just t3
    TmIf t1 t2 t3 -> do
        t1' <- eval1 t1
        return ( TmIf t1' t2 t3 )

    TmSucc t1 -> do
        t1' <- eval1 t1
        return ( TmSucc t1' )

    TmPred TmZero -> Just TmZero
    TmPred ( TmSucc t1 ) -> Just t1
    TmPred t1 -> do
        t1' <-  eval1 t1
        return ( TmPred t1' )

    TmIsZero TmZero -> Just TmTrue
    TmIsZero ( TmSucc n ) | isNumericVal(n) -> Just TmFalse
    TmIsZero t1 -> do
        t1' <- eval1 t1
        return ( TmIsZero t1' )

    _ -> Nothing

eval :: Term -> Term
eval t = case eval1 t of
    Just t' -> eval t'
    Nothing -> t

repl print ts = mapM_ ( print . eval . read ) ts

test = repl ( putStrLn . show ) [
            "TmTrue",
            "TmIf TmFalse TmTrue TmFalse",
            "TmZero",
            "TmSucc (TmPred TmZero)",
            "TmIsZero (TmPred (TmSucc (TmSucc TmZero)))",
            "TmIsZero TmFalse" ]

