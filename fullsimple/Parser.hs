module Parser where

import Core
import qualified Lex as L

import Data.List
import Control.Applicative hiding ((<|>))
import Text.Parsec
import Text.Parsec.String

{-
 - Term := AppTerm | LAMBDA LCID COLON Type DOT Term | IF Term THEN Term ELSE Term
 - AppTerm := ATerm | AppTerm ATerm
 - ATerm := LPAREN Term RPAREN | LCID | TRUE | FALSE
 -
 - Type := ArrowType
 - ArrowType := AType ARROW ArrowType | AType
 - AType := LPAREN Type RPAREN | BOOL
 -
 -}

-- |
-- >>> parseTest (term []) "^x:Bool. x"
-- TmAbs "x" TyBool (TmVar 0 1)
--
-- >>> parseTest (term []) "^x:Bool->Bool. if x true then x false else x true"
-- TmAbs "x" (TyArr TyBool TyBool) (TmIf (TmApp (TmVar 0 1) TmTrue) (TmApp (TmVar 0 1) TmFalse) (TmApp (TmVar 0 1) TmTrue))
--
-- >>> parseTest (term []) "(^x:Bool. x) true"
-- TmApp (TmAbs "x" TyBool (TmVar 0 1)) TmTrue
--
-- >>> parseTest (term []) "if if true then true else true then false else false"
-- TmIf (TmIf TmTrue TmTrue TmTrue) TmFalse TmFalse
--
-- >>> parseTest (seqTerm []) "(^x:Bool. x) true"
-- TmApp (TmAbs "x" TyBool (TmVar 0 1)) TmTrue
--
-- >>> parseTest (seqTerm []) "(^x:Bool. x); true"
-- TmApp (TmAbs "_" TyUnit TmTrue) (TmAbs "x" TyBool (TmVar 0 1))
--
-- >>> parseTest (seqTerm []) "1; 2; 3"
-- TmApp (TmAbs "_" TyUnit (TmApp (TmAbs "_" TyUnit (TmNat 3)) (TmNat 2))) (TmNat 1)
--
-- >>> parseTest (seqTerm []) "(^_x:Bool. true)"
-- TmAbs "_x" TyBool TmTrue
--
-- >>> parseTest (seqTerm []) "(^_:Bool. true)"
-- TmAbs "_" TyBool TmTrue
--
-- >>> parseTest (term []) "let x=true in x"
-- TmLet "x" TmTrue (TmVar 0 1)
--
-- >>> parseTest (term []) "{true, 1}"
-- TmTuple [TmTrue,TmNat 1]
--
-- >>> parseTest (term []) "{true, 1}.1"
-- TmProj 1 (TmTuple [TmTrue,TmNat 1])
--
-- >>> parseTest (term []) "{{true, false}, 1}.1.2"
-- TmProj 2 (TmProj 1 (TmTuple [TmTuple [TmTrue,TmFalse],TmNat 1]))
--
-- >>> parseTest (term []) "(if true then {true, 1} else {false, 0}).1"
-- TmProj 1 (TmIf TmTrue (TmTuple [TmTrue,TmNat 1]) (TmTuple [TmFalse,TmNat 0]))
--
-- >>> parseTest (term []) "{x=1, y=true}"
-- TmRecord [("x",TmNat 1),("y",TmTrue)]
--
-- >>> parseTest (term []) "{x=1, y=true}.x"
-- TmProjRec "x" (TmRecord [("x",TmNat 1),("y",TmTrue)])
term :: Context -> Parser Term
term ctx = appTerm ctx <|> lambda ctx <|> ifThenElse ctx <|> letTerm ctx

letTerm :: Context -> Parser Term
letTerm ctx = do
  name <- L.reserved "let" *> L.identifier
  TmLet name
    <$> ( L.reservedOp "="  *> term ctx )
    <*> ( L.reserved   "in" *> term (addName ctx name) )

seqTerm :: Context -> Parser Term
seqTerm ctx = 
  let (ctx', x') = pickfreshname ctx "_" in
  do
    t1 <- term ctx
    (\t2 -> TmApp (TmAbs x' TyUnit t2) t1) <$> ( L.semi *> seqTerm ctx' ) <|> pure t1

pickfreshname :: Context -> String -> (Context, String)
pickfreshname ctx "_" = (("_", NameBind):ctx, "_")
pickfreshname ctx name = choose names where
  choose (n:ns) = case elemIndex n $ map fst ctx of
    Just _ -> choose ns
    Nothing -> ( (n, NameBind):ctx, n )
  names = name : [ name ++ (show::Integer->String) i | i <- [0..] ]

appTerm :: Context -> Parser Term
appTerm ctx = aTerm ctx `chainL` ( TmApp <$ L.whiteSpace )

atomicTerm :: Context -> Parser Term
atomicTerm ctx = lefty <$> atomic <*> projs where
    atomic =
      try ( L.parens $ term ctx )
      <|> recTerm ctx
      <|> tupleTerm ctx
      <|> identifier ctx
    lefty = foldl (\acc f -> f acc) 
    projs = Text.Parsec.many projTerm

projTerm :: Parser (Term -> Term)
projTerm =
  try (TmProj <$> (L.dot *> L.natural))
  <|>
  (TmProjRec <$> (L.dot *> L.identifier))

tupleTerm :: Context -> Parser Term
tupleTerm ctx = try ( L.braces tuple ) where
  tuple :: Parser Term
  tuple = do
    t0 <- term ctx
    ts <- Text.Parsec.many (L.comma *> term ctx)
    return $ TmTuple (t0:ts)

recTerm :: Context -> Parser Term
recTerm ctx = try ( L.braces record ) where
  record :: Parser Term
  record = do
    u <- unit
    us <- Text.Parsec.many (L.comma *> unit)
    return $ TmRecord (u:us)
  unit = do
    n <- L.identifier
    L.reservedOp "="
    t <- term ctx
    return (n, t)

aTerm :: Context -> Parser Term
aTerm ctx =
  atomicTerm ctx
  <|> TmTrue  <$  L.reserved "true"
  <|> TmFalse <$  L.reserved "false"
  <|> TmUnit  <$  L.reserved "unit"
  <|> TmNat   <$> L.natural

identifier :: Context -> Parser Term
identifier ctx = do
  name <- L.identifier
  case name2index ctx name of
    Just index -> return $ TmVar index (fromIntegral $ length ctx)
    Nothing -> parserFail $ "unknown variable [" ++ name ++ "]"

name2index :: Context -> String -> Maybe Integer
name2index ctx name = fromIntegral <$> elemIndex (name,NameBind) ctx

lambda :: Context -> Parser Term
lambda ctx =  do
  L.reservedOp "^"
  name <- L.identifier
  TmAbs name
    <$> (L.colon *> typeP ctx)
    <*> (L.dot   *> term (addName ctx name))

ifThenElse :: Context -> Parser Term
ifThenElse ctx = TmIf
  <$> ( L.reserved "if"   *> term ctx )
  <*> ( L.reserved "then" *> term ctx )
  <*> ( L.reserved "else" *> term ctx )
  

-- |
-- >>> parseTest (typeP []) "Bool"
-- TyBool
--
-- >>> parseTest (typeP []) "Bool->Bool"
-- TyArr TyBool TyBool
--
-- >>> parseTest (typeP []) "Bool->Bool->Bool"
-- TyArr TyBool (TyArr TyBool TyBool)
--
-- >>> parseTest (typeP []) "(Bool->Bool)->Bool"
-- TyArr (TyArr TyBool TyBool) TyBool
--
typeP :: Context -> Parser Type
typeP = arrowType

arrowType :: Context -> Parser Type
arrowType ctx = do
  ty <- aType ctx
  TyArr ty <$> ( L.reservedOp "->" *> typeP ctx ) <|> pure ty

aType :: Context -> Parser Type
aType ctx =
  try ( L.parens $ typeP ctx )
  <|> L.reserved "Bool" *> return TyBool
  <|> L.reserved "Nat"  *> return TyNat
  <|> L.reserved "Unit" *> return TyUnit

chainL :: Parser a -> Parser (a -> a -> a) -> Parser a
chainL p op = p >>= op `chain` p

chain :: Parser (a -> a -> a) -> Parser a -> a -> Parser a
chain op p l = ((lefty <$> op <*> p) >>= chain op p) <|> pure l
  where
    lefty f r = l `f` r

-- |
-- >>> runTypeof "^x:Bool.x"
-- Right (TyArr TyBool TyBool)
--
-- >>> runTypeof "^x:Bool->Bool. if x true then x (x false) else x false"
-- Right (TyArr (TyArr TyBool TyBool) TyBool)
--
-- >>> runTypeof "(^x:Bool->Bool. x)(^x:Bool->Bool. x)"
-- *** Exception: parameter type mismatch
--
-- >>> runTypeof "(^x:Bool->Bool. x)(^x:Bool. x)"
-- Right (TyArr TyBool TyBool)
--
-- >>> runTypeof "10"
-- Right TyNat
--
-- >>> runTypeof "^x:Nat. x"
-- Right (TyArr TyNat TyNat)
--
-- >>> runTypeof "^x:Bool. if x then 10 else 20"
-- Right (TyArr TyBool TyNat)
--
-- >>> runTypeof "^x:Unit. true"
-- Right (TyArr TyUnit TyBool)
--
-- >>> runTypeof "let x=true in if x then 1 else 2"
-- Right TyNat
--
-- >>> runTypeof "let x=1 in if x then 1 else 2"
-- *** Exception: 条件部が真理値でなかった
--
-- >>> runTypeof "{{true, false}, 1}"
-- Right (TyTuple [TyTuple [TyBool,TyBool],TyNat])
--
-- >>> runTypeof "{{true, false}, 1}.1"
-- Right (TyTuple [TyBool,TyBool])
--
-- >>> runTypeof "{{true, false}, ^x:Bool->Bool. x}"
-- Right (TyTuple [TyTuple [TyBool,TyBool],TyArr (TyArr TyBool TyBool) (TyArr TyBool TyBool)])
--
-- >>> runTypeof "{x=1, y=true}"
-- Right (TyRecord [("x",TyNat),("y",TyBool)])
--
-- >>> runTypeof "{x=1, y=true}.x"
-- Right TyNat
runTypeof :: String -> Either String Type
runTypeof str = case parse (term []) "PARSE ERROR" str of
  Left p  -> fail $ show p
  Right s -> typeof [] s

-- |
-- >>> eval [] <$> readTerm [] "if true then (^x:Bool. x) else (^y:Bool. true)"
-- Right (TmAbs "x" TyBool (TmVar 0 1))
--
-- >>> readTerm [] "10"
-- Right (TmNat 10)
--
-- >>> eval [] <$> readTerm [] "10"
-- Right (TmNat 10)
--
-- >>> eval [] <$> readTerm [] "let x = true in x"
-- Right TmTrue
--
-- >>> eval [] <$> readTerm [] "let x=true in if x then 1 else 2"
-- Right (TmNat 1)
--
-- >>> eval [] <$> readTerm [] "(if true then {true, 1} else {false, 0}).1"
-- Right TmTrue
--
-- >>> eval [] <$> readTerm [] "{{true, false}, 1}.1.2"
-- Right TmFalse
--
-- >>> eval [] <$> readTerm [] "{3, if true then false else false}.2"
-- Right TmFalse
--
-- >>> eval [] <$> readTerm []  "{x=1, y=true}"
-- Right (TmRecord [("x",TmNat 1),("y",TmTrue)])
--
-- >>> eval [] <$> readTerm [] "{x=1, y=true}.x"
-- Right (TmNat 1)
readTerm :: Context -> String -> Either ParseError Term
readTerm ctx = parse (term ctx) "ERR"
