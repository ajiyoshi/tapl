import Core

repl :: Monad m => (Term -> m b) -> [String] -> m ()
repl puts = mapM_ ( puts . eval . read )

test :: IO ()
test = repl ( print . show ) [
            "TmVar 0 0"
            , "TmAbs \"x\" (TmVar 0 0)"
            , "TmApp (TmAbs \"x\" (TmVar 0 0)) (TmAbs \"y\" (TmApp (TmVar 0 0) (TmVar 0 0)))"
            ]

main :: IO ()
main = test
