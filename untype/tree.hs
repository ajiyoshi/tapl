import Data.List
import Text.Parsec
import Text.Parsec.String (Parser)

data Tree = Leaf Integer | Branch Tree Tree deriving (Eq, Show)

data Term = TmVar String Integer Integer | Lambda Term | TmApp Term Term deriving ( Show, Read )
type Identifier = String
type Context = [Identifier]

{-
 - tree := "<" tree "|" tree ">" | num
 - num  := Integer
 -}

tree :: Parser Tree
tree = do
  string "<tree"
  spaces
  left <- tree
  char '|'
  right <- tree
  char '>'
  return (Branch left right)
  <|> num

num :: Parser Tree
num = do
  ds <- many1 digit
  return $ Leaf (read ds)

{-
 - lambda id . Term
 -}

name2index :: Context -> String -> Maybe Integer
name2index ctx name = do
  n <- elemIndex name ctx
  return $ fromIntegral n

term :: Context -> Parser Term
term ctx = appTerm ctx <|> lambda ctx

appTerm :: Context -> Parser Term
appTerm ctx =
  try ( do
    t1 <- atomicTerm ctx
    spaces
    t2 <- appTerm ctx
    return $ TmApp t1 t2 )
  <|>
  atomicTerm ctx

atomicTerm :: Context -> Parser Term
atomicTerm ctx = do
  char '('
  t <- term ctx
  char ')'
  return t
  <|> identifier ctx

lambda :: Context -> Parser Term
lambda ctx = do
  string "^"
  spaces
  name <- many1 letter
  spaces
  char '.'
  spaces
  t <- term $ name : ctx
  return $ Lambda t

identifier ::Context -> Parser Term
identifier ctx = do
  name <- many1 letter
  case name2index ctx name of
    Just index -> return $ TmVar name index (fromIntegral $ length ctx)
    Nothing -> parserFail $ "unknown variable [" ++ name ++ "]"


main :: IO ()
main = do
  parseTest (term []) "(^ x.x)(^ x.x)"
  parseTest (term []) "(^ x.(^y. y x))"
  parseTest (term []) "(^ x . (^y. y y x))"

