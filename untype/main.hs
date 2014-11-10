import Untyped

repl :: Monad m => (Term -> m b) -> [String] -> m ()
repl puts = mapM_ ( puts . eval . read )

test :: IO ()
test = repl ( print . show ) [
            "TmVar 0 0"
            , "TmAbs (TmVar 0 0)"
            , "TmApp (TmAbs (TmVar 0 0)) (TmAbs (TmApp (TmVar 0 0) (TmVar 0 0)))"
            ]

main :: IO ()
main = test
