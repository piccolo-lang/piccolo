{-|
Module        : Core.Parser
Description   : Piccolo-core parser
Stability     : experimental

This is the parser combinators for parsing piccolo-core modules.

__TODO__: define AST construction helpers for ast nodes in ASTUtils and user
them here
-}

module Piccolo.Parser
  ( parseModule
  )
where

import Piccolo.Errors
import Piccolo.AST

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

mkLoc :: SourcePos -> SourcePos -> Location
mkLoc posStart posEnd = Location { locOffset      = -1
                                 , locStartLine   = sourceLine posStart
                                 , locStartColumn = sourceColumn posStart
                                 , locEndLine     = sourceLine posEnd
                                 , locEndColumn   = sourceColumn posStart
                                 }


modul :: Parser Modul
modul = do
  whiteSpace
  pos <- getPosition
  reserved "module"
  modID <- modulId
  defs  <- many definition
  pos' <- getPosition
  eof
  return $ Modul modID defs (mkLoc pos pos')

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
  pos    <- getPosition
  reserved "def"
  name   <- identifier
  params <- parens $ commaSep param
  reserved "="
  body   <- process
  pos'   <- getPosition
  return $ Definition name params body (-1) (mkLoc pos pos')

param :: Parser (String, TypeExpr, Location)
param = do
  pos <- getPosition
  x <- identifier
  reservedOp ":"
  t <- typeExpr
  pos' <- getPosition
  return (x, t, mkLoc pos pos')

process :: Parser Process
process = annotatedProc
      <|> try (do
             pos <- getPosition
             reserved "end"
             pos' <- getPosition
             return (PEnd (mkLoc pos pos')))
      <|> braces process
      <|> try (do
             pos <- getPosition
             a <- action
             reservedOp ","
             p <- process
             pos' <- getPosition
             return $ PPrefix a p (mkLoc pos pos')
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
  pos <- getPosition
  m <- option "" modulQual -- TODO: optionMaybe
  p <- identifier
  args <- parens $ commaSep expr
  pos' <- getPosition
  return $ PCall m p args (mkLoc pos pos')

guardedChoice :: Parser Process
guardedChoice = do
  pos <- getPosition
  bs <- sepBy1 branch $ between whiteSpace whiteSpace (char '+')
  pos' <- getPosition
  return $ PChoice bs False (mkLoc pos pos')

branch :: Parser Branch
branch = try branchTau
     <|> try branchOutput
     <|> try branchInput
     <?> "choice branch"

branchTau :: Parser Branch
branchTau = do
  pos <- getPosition
  g <- brackets expr
  reserved "tau"
  reservedOp ","
  p <- process
  pos' <- getPosition
  return $ BTau g p (mkLoc pos pos')

branchOutput :: Parser Branch
branchOutput = do
  pos <- getPosition
  g <- brackets expr
  c <- identifier
  reservedOp "!"
  e <- expr
  reservedOp ","
  p <- process
  pos' <- getPosition
  return $ BOutput g c e (-1) p (mkLoc pos pos')

branchInput :: Parser Branch
branchInput = do
  pos <- getPosition
  g <- brackets expr
  c <- identifier
  reservedOp "?"
  x <- parens identifier
  reservedOp ","
  p <- process
  pos' <- getPosition
  return $ BInput g c x (TUnknown noLoc) (-1) (-1) p (mkLoc pos pos')

action :: Parser Action
action = try output
     <|> try input
     <|> (do
            pos <- getPosition
            reserved "new"
            (v, t) <- parens typedVar
            pos' <- getPosition
            return (ANew v (-1) t (mkLoc pos pos')))
     <|> do
           pos <- getPosition
           reserved "let"
           (v, t, e) <- parens $ do
             v <- identifier
             reservedOp ":"
             t <- typeExpr
             reservedOp "="
             e <- expr
             return (v, t, e)
           pos' <- getPosition
           return $ ALet v (-1) t e (mkLoc pos pos')
     <|> do
           pos <- getPosition
           reserved "spawn"
           PCall m k vs _ <- braces call
           pos' <- getPosition
           return $ ASpawn m k vs (mkLoc pos pos')
     <|> do
           pos <- getPosition
           m <- modulQual
           n <- identifier
           args <- parens $ commaSep expr
           pos' <- getPosition
           return $ APrim m n args (mkLoc pos pos')
     <?> "action"

output :: Parser Action
output = do
  pos <- getPosition
  c <- identifier
  reservedOp "!"
  e <- expr
  pos' <- getPosition
  return $ AOutput c e (-1) (mkLoc pos pos')

input :: Parser Action
input = do
  pos <- getPosition
  c <- identifier
  reservedOp "?"
  x <- parens identifier
  pos' <- getPosition
  return $ AInput c x (TUnknown noLoc) (-1) (-1) (mkLoc pos pos')

typeExpr :: Parser TypeExpr
typeExpr = (do
              pos <- getPosition
              reserved "chan"
              t <- angles typeExpr
              pos' <- getPosition
              return $ TChannel t (mkLoc pos pos'))
       <|> (do
              pos <- getPosition
              ts <- parens $ sepBy1 typeExpr (char '*')
              pos' <- getPosition
              return $ TTuple ts (mkLoc pos pos'))
       <|> (do
              pos <- getPosition
              at <- typeAtom
              pos' <- getPosition
              return $ TAtom at (mkLoc pos pos'))
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
   <|> (do
          pos <- getPosition
          s <- stringLiteral
          pos' <- getPosition
          return $ EString (TUnknown noLoc) (mkLoc pos pos') s)
   <|> (do
          pos <- getPosition
          es <- parens $ commaSep1 expr
          pos' <- getPosition
          return $ ETuple (TUnknown noLoc) (mkLoc pos pos') es)
   <|> varExpr
   <|> (do
          pos <- getPosition
          m <- modulQual
          n <- identifier
          args <- parens $ commaSep staticExpr
          pos' <- getPosition
          return $ EPrim (TUnknown noLoc) (mkLoc pos pos') m n args
       )
   <?> "expression"

staticExpr :: Parser Expr
staticExpr = (try trueExpr)
         <|> (try falseExpr)
         <|> intExpr
         <|> varExpr
         <?> "static expression"

trueExpr :: Parser Expr
trueExpr = do
  pos <- getPosition
  reserved "true"
  pos' <- getPosition
  return (ETrue (TUnknown noLoc) (mkLoc pos pos'))

falseExpr :: Parser Expr
falseExpr = do
  pos <- getPosition
  reserved "false"
  pos' <- getPosition
  return (EFalse (TUnknown noLoc) (mkLoc pos pos'))

intExpr :: Parser Expr
intExpr = do
  pos <- getPosition
  i <- integer
  pos' <- getPosition
  return $ EInt (TUnknown noLoc) (mkLoc pos pos') (fromIntegral i)

varExpr :: Parser Expr
varExpr = do
  pos <- getPosition
  v <- identifier
  pos' <- getPosition
  return $ EVar (TUnknown noLoc) (mkLoc pos pos') v (-1)


-- | Module parser
parseModule :: String -> Either PiccError Modul
parseModule s = left (ParsingError . show) $ parse modul "Module Parser" s
