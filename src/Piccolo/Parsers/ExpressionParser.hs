{-|
Module        : Core.Parser
Description   : Piccolo-core parser
Stability     : experimental

These are the parser combinators for parsing piccolo-core modules.
-}

module Piccolo.Parsers.ExpressionParser
  ( typeExpr
  , typedVar
  , expr
  )
where

import Piccolo.AST
import Piccolo.Parsers.Lexer

import Text.Parsec hiding (string)
import Text.Parsec.String (Parser)
import Text.Parsec.Language ()


typeExpr :: Parser TypeExpr
typeExpr = (withLocation $ do
              reserved "chan"
              t <- angles typeExpr
              return $ TChannel t
           )
       <|> (withLocation $ parens $ sepBy1 typeExpr (char '*') >>= return . TTuple)
       <|> (withLocation $ typeAtom >>= return . TAtom)
       <?> "type"

typeAtom :: Parser TypeAtom
typeAtom = (reserved "bool"   >> return TBool)
       <|> (reserved "int"    >> return TInt)
       <|> (reserved "string" >> return TString)
       <?> "type atom"

typedVar :: Parser (String, TypeExpr)
typedVar = do
  v <- identifier
  reservedOp ":"
  t <- typeExpr
  return (v, t)

expr :: Parser Expr
expr = orExpr

orExpr :: Parser Expr
orExpr = chainl1 andExpr $ do
  reservedOp "||"
  return $ EOr (TUnknown noLoc) noLoc

andExpr :: Parser Expr
andExpr = chainl1 expr' $ do
  reservedOp "&&"
  return $ EAnd (TUnknown noLoc) noLoc

expr' :: Parser Expr
expr' = try trueExpr
   <|> try falseExpr
   <|> intExpr
   <|> (withLocation $ do
          s <- stringLiteral
          return $ \p -> EString (TUnknown noLoc) p s
       )
   <|> (withLocation $ do
          es <- parens $ commaSep1 expr
          return $ \p -> ETuple (TUnknown noLoc) p es
       )
   <|> varExpr
   <|> (withLocation $ do
          m <- modulQual
          n <- identifier
          args <- parens $ commaSep staticExpr
          return $ \p -> EPrim (TUnknown noLoc) p m n args
       )
   <?> "expression"

staticExpr :: Parser Expr
staticExpr = try trueExpr
         <|> try falseExpr
         <|> intExpr
         <|> varExpr
         <?> "static expression"

trueExpr :: Parser Expr
trueExpr = withLocation $ reserved "true" >> return (ETrue (TUnknown noLoc))

falseExpr :: Parser Expr
falseExpr = withLocation $ reserved "false" >> return (EFalse (TUnknown noLoc))

intExpr :: Parser Expr
intExpr = withLocation $ do
  i <- fmap fromIntegral integer
  return $ \p -> EInt (TUnknown noLoc) p i

varExpr :: Parser Expr
varExpr = withLocation $ do
  v <- identifier
  return $ \p -> EVar (TUnknown noLoc) p v (-1)
