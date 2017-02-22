{-|
Module        : Piccolo.Parsers.ProcessParser
Description   : Piccolo process parser
Stability     : experimental

These are the parser combinators for parsing piccolo processes.
-}

module Piccolo.Parsers.ProcessParser
  ( process
  , parseProcess
  )
where

import Piccolo.AST
import Piccolo.Errors
import Piccolo.Parsers.ExpressionParser
import Piccolo.Parsers.Lexer
import Piccolo.Parsers.Utils

import Control.Arrow
import Text.Parsec hiding (string)
import Text.Parsec.String (Parser)
import Text.Parsec.Language ()


process :: Parser Process
process = annotatedProc
      <|> try endProc
      <|> braces process
      <|> try prefixedProc
      <|> try call
      <|> guardedChoice
      <?> "process"

annotatedProc :: Parser Process
annotatedProc = do
  reserved "%safe"
  p <- braces process
  case p of
    c@PChoice {} -> return c { procSafe = True }
    _            -> unexpected "%safe should be applied on choice construction"
  
endProc :: Parser Process
endProc = withLocation $ reserved "end" >> return PEnd

prefixedProc :: Parser Process
prefixedProc = withLocation $ do
  act  <- action
  reservedOp ","
  proc <- process
  return $ PPrefix act proc

call :: Parser Process
call = withLocation $ do
  m    <- option (ModuleName []) modulQual
  n    <- identifier
  args <- parens $ commaSep expr
  return $ PCall m n args

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
  guard <- brackets expr
  AOutput c e _ _ <- output
  reservedOp ","
  proc  <- process
  return $ BOutput guard c e (-1) proc

branchInput :: Parser Branch
branchInput = withLocation $ do
  guard <- brackets expr
  AInput c x _ _ _ _ <- input
  reservedOp ","
  proc <- process
  return $ BInput guard c x noTyp (-1) (-1) proc

action :: Parser Action
action = try output
     <|> try input
     <|> try newAct
     <|> try letAct
     <|> try spawn
     <|> try prim
     <?> "action"

newAct :: Parser Action
newAct = withLocation $ do
  reserved "new"
  (v,t) <- parens typedVar
  return $ ANew v (-1) t

letAct :: Parser Action
letAct = withLocation $ do
  reserved "let"
  (v,t,e) <- parens $ do
    (v,t) <- typedVar
    reservedOp "="
    e <- expr
    return (v,t,e)
  return $ ALet v (-1) t e

spawn :: Parser Action
spawn = withLocation $ do
  reserved "spawn"
  PCall m k vs _ <- braces call
  return $ ASpawn m k vs

prim :: Parser Action
prim = withLocation $ do
  m    <- modulQual
  n    <- identifier
  args <- parens $ commaSep expr
  return $ APrim m n args

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
  return $ AInput c x noTyp (-1) (-1)

-- | Process parser
parseProcess :: String -> Either PiccError Process
parseProcess s = left (ParsingError . show) $ parse process "<stdin>" s
