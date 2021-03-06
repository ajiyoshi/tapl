module Parser where

import Core
import Syntax
import qualified Lex as L

import Data.List (foldl')
import Control.Applicative ((<$>), (<$), (<*>), (*>), (<*), pure )
import Control.Arrow (first)
import Text.Parsec -- (ParseError, parse, (<|>), many, try, parserFail, parseTest)
import Text.Parsec.String (Parser)
--import Text.Parsec.Char (upper)

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

toplevel :: Context -> Parser ([Command], Context)
toplevel ctx = 
  ( eof *> return ([], ctx) )
  <|>
  do
    (cmd, ctx') <- command ctx <* L.semi
    first ((:) cmd) <$> toplevel ctx' <|> pure ([cmd], ctx')

command :: Context -> Parser (Command, Context)
command ctx =
  try ( (\t -> (Eval t, ctx)) <$> term ctx )
  <|>
  (\(n, b) -> (Bind n b, addBinding ctx n b)) <$> binder ctx

-- |
-- >>> parseTest (binder []) "BB = Bool->Bool"
-- ("BB",TyAbbBind (TyArr TyBool TyBool))
--
-- >>> parseTest (binder []) "a"
-- ("a",TyVarBind)
--
-- >>> parseTest (binder []) "a = ^x:Bool. x"
-- ("a",TmAbbBind (TmAbs "x" TyBool (TmVar 0 1)) Nothing)
--
-- >>> parseTest (binder []) "a = ^x:Bool. x :: Bool->Bool"
-- ("a",TmAbbBind (TmAbs "x" TyBool (TmVar 0 1)) (Just (TyArr TyBool TyBool)))
--
binder :: Context -> Parser (String, Binding)
binder ctx =
  try ( (\n x -> (n, TyAbbBind x)) <$> (upperId <* L.reservedOp "=") <*> typeP ctx )
  <|> try ( (\n x y -> (n, TmAbbBind x (Just y))) <$> (L.identifier <* L.reservedOp "=") <*> term ctx <*> (L.reservedOp "::" *> typeP ctx) )
  <|> try ( (\n x -> (n, TmAbbBind x Nothing)) <$> (L.identifier <* L.reservedOp "=") <*> term ctx )
  <|> try ( (\n -> (n, TyVarBind)) <$> L.identifier )
             
upperId :: Parser String
upperId = (:) <$> upper <*> L.identifier

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
--
-- >>> parseTest (term []) "{}"
-- TmTuple []
--
-- >>> parseTest (term []) "iszero(pred (succ 1))"
-- TmIszero (TmPred (TmSucc (TmNat 1)))
term :: Context -> Parser Term
term ctx = appTerm ctx <|> lambda ctx <|> ifThenElse ctx <|> letTerm ctx <|> applyTerm ctx

applyTerm :: Context -> Parser Term
applyTerm ctx = 
  TmSucc <$> (L.reserved "succ" *> term ctx)
  <|> TmPred <$> (L.reserved "pred" *> term ctx)
  <|> TmIszero <$> (L.reserved "iszero" *> term ctx)
  <|> TmFix <$> (L.reserved "fix" *> term ctx)

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

appTerm :: Context -> Parser Term
appTerm ctx = aTerm ctx `chainL` ( TmApp <$ L.whiteSpace )

atomicTerm :: Context -> Parser Term
atomicTerm ctx = foldl' (\acc f -> f acc) <$> atomic <*> projs where
    atomic =
      try ( L.parens $ seqTerm ctx )
      <|> tupleTerm ctx
      <|> recTerm ctx
      <|> identifier ctx
    projs = many projTerm

projTerm :: Parser (Term -> Term)
projTerm = do
  L.dot
  ( TmProj <$> L.natural ) <|> ( TmProjRec <$> L.identifier )

tupleTerm :: Context -> Parser Term
tupleTerm ctx = try ( L.braces (TmTuple <$> L.commaSep (term ctx)) )

recTerm :: Context -> Parser Term
recTerm ctx = try ( L.braces (TmRecord <$> L.commaSep unit) ) where
  unit :: Parser (String, Term)
  unit = (\n t -> (n, t)) <$> L.identifier <*> (L.reservedOp "=" *> term ctx)

aTerm :: Context -> Parser Term
aTerm ctx = atomicTerm ctx <|> literalTerm

literalTerm :: Parser Term
literalTerm = 
  TmNat <$> L.natural
  <|> TmTrue  <$ L.reserved "true"
  <|> TmFalse <$ L.reserved "false"
  <|> TmUnit  <$ L.reserved "unit"

identifier :: Context -> Parser Term
identifier ctx = do
  name <- L.identifier
  case name2index ctx name of
    Just index -> return $ TmVar index (fromIntegral $ length ctx)
    Nothing -> parserFail $ "unknown variable [" ++ name ++ "]"

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
-- >>> parseTest (typeP []) "{Bool, Bool}"
-- TyTuple [TyBool,TyBool]
--
-- >>> parseTest (typeP []) "{}"
-- TyTuple []
--
-- >>> parseTest (typeP []) "{x=Bool, y=Bool->Bool}"
-- TyRecord [("x",TyBool),("y",TyArr TyBool TyBool)]
--
typeP :: Context -> Parser Type
typeP ctx = arrowType ctx <|> tupleType ctx <|> recType ctx

tupleType :: Context -> Parser Type
tupleType ctx = try ( L.braces (TyTuple <$> L.commaSep (typeP ctx)) )

recType :: Context -> Parser Type
recType ctx = try ( L.braces (TyRecord <$> L.commaSep unit) ) where
  unit :: Parser (String, Type)
  unit = (\n t -> (n, t)) <$> L.identifier <*> (L.reservedOp "=" *> typeP ctx)

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
  <|> varType ctx

varType :: Context -> Parser Type
varType ctx = do
  name <- L.identifier
  case name2index ctx name of
    Just index -> return $ TyVar index (fromIntegral $ length ctx)
    Nothing -> parserFail $ "unknown variable [" ++ name ++ "]"

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
--
-- >>> runTypeof "((^x:{x=Nat, y=Nat}. {false, x}) {x=1, y=2})"
-- Right (TyTuple [TyBool,TyRecord [("x",TyNat),("y",TyNat)]])
--
-- >>> runTypeof "((^x:{x=Nat, y=Nat}. {false, x}) {1, 2})"
-- *** Exception: parameter type mismatch
--
-- >>> runTypeof "((^x:{x=Nat, y=Nat}. x.y) {x=1, y=2})"
-- Right TyNat
--
-- >>> runTypeof "iszero(pred (succ 1))"
-- Right TyBool
--
-- >>> runTypeof "pred false"
-- *** Exception: pred needs nat val
--
-- >>> runTypeof "succ (if iszero 0 then pred 10 else succ 3)"
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
--
-- >>> eval [] <$> readTerm [] "^x:{Bool, Bool}. {false, x}"
-- Right (TmAbs "x" (TyTuple [TyBool,TyBool]) (TmTuple [TmFalse,TmVar 0 1]))
--
-- >>> eval [] <$> readTerm [] "((^x:{Bool, Bool}. {false, x}) {true, false})"
-- Right (TmTuple [TmFalse,TmTuple [TmTrue,TmFalse]])
--
-- >>> eval [] <$> readTerm [] "^x:{x=Nat, y=Nat}. {false, x}"
-- Right (TmAbs "x" (TyRecord [("x",TyNat),("y",TyNat)]) (TmTuple [TmFalse,TmVar 0 1]))
--
-- >>> eval [] <$> readTerm [] "((^x:{x=Nat, y=Nat}. {false, x}) {x=1, y=2})"
-- Right (TmTuple [TmFalse,TmRecord [("x",TmNat 1),("y",TmNat 2)]])
--
-- >>> eval [] <$> readTerm [] "((^x:{x=Nat, y=Nat}. x.y) {x=1, y=2})"
-- Right (TmNat 2)
--
-- >>> eval [] <$> readTerm [] "succ (if iszero 0 then pred 10 else succ 3)"
-- Right (TmNat 10)
--
--
readTerm :: Context -> String -> Either ParseError Term
readTerm ctx = parse (term ctx) "ERR"

appendShortName :: String -> String -> String
appendShortName s s' = foldl' (flip (:)) s' $ reverse s

showTypeS :: Context -> Type -> String -> String
showTypeS _ = shows

showTermS :: Context -> Term -> String -> String
showTermS ctx t0 = case t0 of
  TmAbs n ty t1 -> ('^':) . appendShortName n . (':':) . showTypeS ctx ty . ('.':) . showTermS (addName ctx n) t1
  TmApp t1 t2 -> ('(':) . showTermS ctx t1 . (' ':) . showTermS ctx t2 . (')':)
  TmIf t1 t2 t3 ->
    appendShortName "if " . showTermS ctx t1
    . appendShortName " then " . showTermS ctx t2
    . appendShortName " else " . showTermS ctx t3
  TmNat n -> shows n
  TmTrue -> shows "true"
  TmFalse -> shows "false"
  t1 -> shows t1
  

