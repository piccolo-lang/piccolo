{-|
Module        : Piccolo.Parsers.ExpressionParser
Description   : Piccolo expression parser
Stability     : experimental

These are the parser combinators for parsing piccolo expressions.
-}

module Piccolo.Parsers.ExpressionParser
  ( typeExpr
  , typedVar
  , expr
  , parseExpression
  )
where

import Piccolo.AST
import Piccolo.Errors
import Piccolo.Parsers.Lexer
import Piccolo.Parsers.Utils

import Control.Arrow
import Data.Functor.Identity
import Text.Parsec hiding (string)
import Text.Parsec.String (Parser)
import Text.Parsec.Language ()
import Text.Parsec.Expr


typeExpr :: Parser TypeExpr
typeExpr = typeChan
       <|> typeTuple
       <|> typeAtom
       <?> "type"

typeChan :: Parser TypeExpr
typeChan = withLocation $ do
  reserved "chan"
  t <- angles typeExpr
  return $ TChannel t

typeTuple :: Parser TypeExpr
typeTuple = withLocation $ do
  ts <- parens $ sepBy1 typeExpr (char '*')
  return $ TTuple ts

typeAtom :: Parser TypeExpr
typeAtom = withLocation $ boolType <|> intType <|> stringType
  where boolType   = reserved "bool"   >> return (TAtom TBool)
        intType    = reserved "int"    >> return (TAtom TInt)
        stringType = reserved "string" >> return (TAtom TString)

typedVar :: Parser (String, TypeExpr)
typedVar = do
  v <- identifier
  reservedOp ":"
  t <- typeExpr
  return (v,t)

binary :: String ->
          (Location -> Expr -> Expr -> Expr) ->
          Assoc ->
          Operator String () Identity Expr
binary s f = Infix (withLocation $ reservedOp s >> return f)

binaryPrim :: String ->
              ModuleName ->
              String ->
              Assoc ->
              Operator String () Identity Expr
binaryPrim s m p = Infix (withLocation $ do
  reservedOp s
  return $ \l x y -> EPrim noTyp l m p [x,y])

table :: OperatorTable String () Identity Expr
table = [ [ binaryPrim "*"  arith "mul" AssocLeft
          , binaryPrim "/"  arith "div" AssocLeft
          , binaryPrim "%"  arith "mod" AssocLeft ]
        , [ binaryPrim "+"  arith "add" AssocLeft
          , binaryPrim "-"  arith "sub" AssocLeft ]
        , [ binaryPrim "<=" arith "less_or_eq_than" AssocLeft
          , binaryPrim "<"  arith "less_than" AssocLeft
          , binaryPrim ">=" arith "greater_or_eq_than" AssocLeft
          , binaryPrim ">"  arith "greater_than" AssocLeft ]
        , [ binaryPrim "==" arith "equals" AssocLeft
          , binaryPrim "!=" arith "not_equals" AssocLeft ]
        , [ binary "&&" (EAnd noTyp) AssocLeft ]
        , [ binary "||" (EOr noTyp)  AssocLeft ]
        ]
  where arith = ModuleName ["core", "arith"]

expr :: Parser Expr
expr = buildExpressionParser table expr'

expr' :: Parser Expr
expr' = try trueExpr
    <|> try falseExpr
    <|> intExpr
    <|> stringExpr
    <|> tupleExpr
    <|> varExpr
    <|> primExpr
    <?> "expression"

staticExpr :: Parser Expr
staticExpr = try trueExpr
         <|> try falseExpr
         <|> intExpr
         <|> varExpr
         <?> "static expression"

trueExpr :: Parser Expr
trueExpr = withLocation $ reserved "true" >> return (ETrue noTyp)

falseExpr :: Parser Expr
falseExpr = withLocation $ reserved "false" >> return (EFalse noTyp)

intExpr :: Parser Expr
intExpr = withLocation $ do
  i <- fmap fromIntegral integer
  return $ \p -> EInt noTyp p i

stringExpr :: Parser Expr
stringExpr = withLocation $ do
  s <- stringLiteral
  return $ \p -> EString noTyp p s

tupleExpr :: Parser Expr
tupleExpr = withLocation $ do
  es <- parens $ commaSep1 expr
  return $ \p -> ETuple noTyp p es

varExpr :: Parser Expr
varExpr = withLocation $ do
  v <- identifier
  return $ \p -> EVar noTyp p v (-1)

primExpr :: Parser Expr
primExpr = withLocation $ do
  m    <- modulQual
  n    <- identifier
  args <- parens $ commaSep staticExpr
  return $ \p -> EPrim noTyp p m n args

-- | Expression parser
parseExpression :: String -> Either PiccError Expr
parseExpression s = left (ParsingError . show) $ parse expr "<stdin>" s
