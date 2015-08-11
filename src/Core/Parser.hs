{-|
Module        : Core.Parser
Description   : Piccolo-core parser
Stability     : experimental

This is the parser combinators for parsing piccolo-core modules.

__TODO__: feed the locations to the AST
__TODO__: define AST construction helpers for ast nodes in ASTUtils and user
them here
-}

module Core.Parser
  ( parseModule
  )
where

import Errors
import Core.AST

import Text.Parsec hiding (string)
import Text.Parsec.String (Parser)
import Text.Parsec.Language ()
import qualified Text.Parsec.Token as Tok

import Control.Arrow


langDef :: Tok.LanguageDef ()
langDef = Tok.LanguageDef
  { Tok.commentStart    = "/*"
  , Tok.commentEnd      = "*/"
  , Tok.commentLine     = "//"
  , Tok.nestedComments  = True
  , Tok.identStart      = letter
  , Tok.identLetter     = alphaNum <|> char '_'
  , Tok.opStart         = oneOf ""
  , Tok.opLetter        = oneOf ""
  , Tok.reservedNames   = [ "true", "false", "module", "def", "end", "tau"
                          , "new", "spawn", "let", "and", "or"
                          , "chan", "int", "bool", "string"
                          , "%safe"
                          ]
  , Tok.reservedOpNames = [ ",", "?", "!", "/", "=", ":", "#" ]
  , Tok.caseSensitive   = True
  }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

identifier :: Parser String
identifier = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

angles :: Parser a -> Parser a
angles = Tok.angles lexer

braces :: Parser a -> Parser a
braces = Tok.braces lexer

brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

commaSep1 :: Parser a -> Parser [a]
commaSep1 = Tok.commaSep1 lexer

integer :: Parser Integer
integer = Tok.integer lexer

stringLiteral :: Parser String
stringLiteral = Tok.stringLiteral lexer


modul :: Parser Modul
modul = do
  whiteSpace
  reserved "module"
  modID <- modulId
  defs  <- many definition
  eof
  return $ Modul modID defs noLoc

modulId :: Parser String
modulId = do
  m <- identifier
  _ <- reservedOp "/"
  n <- identifier
  return $ m ++ "/" ++ n

modulQual :: Parser String
modulQual = do
  reservedOp "#"
  m <- modulId
  reservedOp ":"
  return m

definition :: Parser Definition
definition = do
  reserved "def"
  name   <- identifier
  params <- parens $ commaSep param
  reserved "="
  body   <- process
  return $ Definition name params body (-1) noLoc

param :: Parser (String, TypeExpr, Location)
param = do
  x <- identifier
  reservedOp ":"
  t <- typeExpr
  return (x, t, noLoc)

process :: Parser Process
process = annotatedProc
      <|> try (reserved "end" >> return (PEnd noLoc))
      <|> braces process
      <|> try (do
             a <- action
             reservedOp ","
             p <- process
             return $ PPrefix a p noLoc
          )
      <|> try call
      <|> guardedChoice
      <?> "process"

annotatedProc :: Parser Process
annotatedProc = do
  reserved "%safe"
  p <- braces process
  case p of
    c@(PChoice {}) -> return c { procSafe = True }
    _              -> unexpected "%safe should be applied on choice"
  
call :: Parser Process
call = do
  m <- option "" modulQual -- TODO: optionMaybe
  p <- identifier
  args <- parens $ commaSep expr
  return $ PCall m p args noLoc

guardedChoice :: Parser Process
guardedChoice = do
  bs <- sepBy1 branch $ between whiteSpace whiteSpace (char '+')
  return $ PChoice bs False noLoc

branch :: Parser Branch
branch = try branchTau
     <|> try branchOutput
     <|> try branchInput
     <?> "choice branch"

branchTau :: Parser Branch
branchTau = do
  g <- brackets expr
  reserved "tau"
  reservedOp ","
  p <- process
  return $ BTau g p noLoc

branchOutput :: Parser Branch
branchOutput = do
  g <- brackets expr
  c <- identifier
  reservedOp "!"
  e <- expr
  reservedOp ","
  p <- process
  return $ BOutput g c e (-1) p noLoc

branchInput :: Parser Branch
branchInput = do
  g <- brackets expr
  c <- identifier
  reservedOp "?"
  x <- parens identifier
  reservedOp ","
  p <- process
  return $ BInput g c x (TUnknown noLoc) (-1) (-1) p noLoc

action :: Parser Action
action = try output
     <|> try input
     <|> (reserved "new" >> parens typedVar >>= \(v,t) ->
           return (ANew v (-1) t noLoc))
     <|> do
           reserved "let"
           (v, t, e) <- parens $ do
             v <- identifier
             reservedOp ":"
             t <- typeExpr
             reservedOp "="
             e <- expr
             return (v, t, e)
           return $ ALet v (-1) t e noLoc
     <|> do
           reserved "spawn"
           PCall m k vs _ <- braces call
           return $ ASpawn m k vs noLoc
     <|> do
           m <- modulQual
           n <- identifier
           args <- parens $ commaSep expr
           return $ APrim m n args noLoc
     <?> "action"

output :: Parser Action
output = do
  c <- identifier
  reservedOp "!"
  e <- expr
  return $ AOutput c e (-1) noLoc

input :: Parser Action
input = do
  c <- identifier
  reservedOp "?"
  x <- parens identifier
  return $ AInput c x (TUnknown noLoc) (-1) (-1) noLoc

typeExpr :: Parser TypeExpr
typeExpr = (reserved "chan" >> angles typeExpr >>=
            \t -> return $ TChannel t noLoc)
       <|> (parens $ sepBy1 typeExpr (char '*') >>=
            \ts -> return $ TTuple ts noLoc)
       <|> (typeAtom >>= \at -> return $ TAtom at noLoc)
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

expr :: Parser Expr -- TODO: 'and' and 'or' parsers
expr = (try trueExpr)
   <|> (try falseExpr)
   <|> intExpr
   <|> (stringLiteral >>= \s -> return $ EString (TUnknown noLoc) noLoc s)
   <|> (parens $ commaSep1 expr >>= \es -> return $ ETuple (TUnknown noLoc) noLoc es)
   <|> varExpr
   <|> (do
          m <- modulQual
          n <- identifier
          args <- parens $ commaSep staticExpr
          return $ EPrim (TUnknown noLoc) noLoc m n args
       )
   <?> "expression"

staticExpr :: Parser Expr
staticExpr = (try trueExpr)
         <|> (try falseExpr)
         <|> intExpr
         <|> varExpr

trueExpr :: Parser Expr
trueExpr = reserved "true" >> return (ETrue (TUnknown noLoc) noLoc)

falseExpr :: Parser Expr
falseExpr = reserved "false" >> return (EFalse (TUnknown noLoc) noLoc)

intExpr :: Parser Expr
intExpr = integer >>= \i -> return $ EInt (TUnknown noLoc) noLoc (fromIntegral i)

varExpr :: Parser Expr
varExpr = identifier >>= \v -> return $ EVar (TUnknown noLoc) noLoc v (-1)


-- | Module parser
parseModule :: String -> Either PiccError Modul
parseModule s = left (ParsingError . show) $ parse modul "Module Parser" s
