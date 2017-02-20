{-|
Module        : Core.Parser
Description   : Piccolo-core parser
Stability     : experimental

These are the parser combinators for parsing piccolo-core modules.
-}

module Piccolo.Parsers.ProcessParser
  ( process
  )
where

import Piccolo.AST
import Piccolo.Parsers.ExpressionParser
import Piccolo.Parsers.Utils

import Text.Parsec hiding (string)
import Text.Parsec.String (Parser)
import Text.Parsec.Language ()


process :: Parser Process
process = annotatedProc
      <|> try (withLocation $ reserved "end" >> return PEnd)
      <|> braces process
      <|> try (withLocation $ do
             a <- action
             reservedOp ","
             p <- process
             return $ PPrefix a p
          )
      <|> try call
      <|> guardedChoice
      <?> "process"

annotatedProc :: Parser Process
annotatedProc = do
  reserved "%safe"
  p <- braces process
  case p of
    c@PChoice {} -> return c { procSafe = True }
    _            -> unexpected "%safe should be applied on choice"
  
call :: Parser Process
call = withLocation $ do
  m <- option (ModuleName []) modulQual
  p <- identifier
  args <- parens $ commaSep expr
  return $ PCall m p args

guardedChoice :: Parser Process
guardedChoice = withLocation $ do
  bs <- sepBy1 branch $ between whiteSpace whiteSpace (char '+')
  return $ PChoice bs False

branch :: Parser Branch
branch = try branchTau
     <|> try branchOutput
     <|> try branchInput
     <?> "choice branch"

branchTau :: Parser Branch
branchTau = withLocation $ do
  g <- brackets expr
  reserved "tau"
  reservedOp ","
  p <- process
  return $ BTau g p

branchOutput :: Parser Branch
branchOutput = withLocation $ do
  g <- brackets expr
  c <- identifier
  reservedOp "!"
  e <- expr
  reservedOp ","
  p <- process
  return $ BOutput g c e (-1) p

branchInput :: Parser Branch
branchInput = withLocation $ do
  g <- brackets expr
  c <- identifier
  reservedOp "?"
  x <- parens identifier
  reservedOp ","
  p <- process
  return $ BInput g c x (TUnknown noLoc) (-1) (-1) p

action :: Parser Action
action = try output
     <|> try input
     <|> (withLocation $ do
            reserved "new"
            (v, t) <- parens typedVar
            return $ ANew v (-1) t
         )
     <|> (withLocation $ do
           reserved "let"
           (v, t, e) <- parens $ do
             v <- identifier
             reservedOp ":"
             t <- typeExpr
             reservedOp "="
             e <- expr
             return (v, t, e)
           return $ ALet v (-1) t e
         )
     <|> (withLocation $ do
           reserved "spawn"
           PCall m k vs _ <- braces call
           return $ ASpawn m k vs
         )
     <|> (withLocation $ do
           m <- modulQual
           n <- identifier
           args <- parens $ commaSep expr
           return $ APrim m n args
         )
     <?> "action"

output :: Parser Action
output = withLocation $ do
  c <- identifier
  reservedOp "!"
  e <- expr
  return $ AOutput c e (-1)

input :: Parser Action
input = withLocation $ do
  c <- identifier
  reservedOp "?"
  x <- parens identifier
  return $ AInput c x (TUnknown noLoc) (-1) (-1)

