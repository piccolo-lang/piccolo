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

applyLocation :: Parser (Location -> a) -> Parser a
applyLocation parser = do
  pos <- getPosition
  x <- parser
  pos' <- getPosition
  return $ x (mkLoc pos pos')

modul :: Parser Modul
modul = do
  whiteSpace
  m <- applyLocation $ do
    reserved "module"
    modID <- modulId
    defs <- many definition
    return $ Modul modID defs
  eof
  return m

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
definition = applyLocation $ do
  reserved "def"
  name   <- identifier
  params <- parens $ commaSep param
  reserved "="
  body   <- process
  return $ Definition name params body (-1) (-1)

param :: Parser (String, TypeExpr, Location)
param = applyLocation $ do
  x <- identifier
  reservedOp ":"
  t <- typeExpr
  return $ \p -> (x, t, p)

process :: Parser Process
process = annotatedProc
      <|> try (applyLocation $ reserved "end" >> return PEnd)
      <|> braces process
      <|> try (applyLocation $ do
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
call = applyLocation $ do
  m <- option "" modulQual -- TODO: optionMaybe
  p <- identifier
  args <- parens $ commaSep expr
  return $ PCall m p args

guardedChoice :: Parser Process
guardedChoice = applyLocation $ do
  bs <- sepBy1 branch $ between whiteSpace whiteSpace (char '+')
  return $ PChoice bs False

branch :: Parser Branch
branch = try branchTau
     <|> try branchOutput
     <|> try branchInput
     <?> "choice branch"

branchTau :: Parser Branch
branchTau = applyLocation $ do
  g <- brackets expr
  reserved "tau"
  reservedOp ","
  p <- process
  return $ BTau g p

branchOutput :: Parser Branch
branchOutput = applyLocation $ do
  g <- brackets expr
  c <- identifier
  reservedOp "!"
  e <- expr
  reservedOp ","
  p <- process
  return $ BOutput g c e (-1) p

branchInput :: Parser Branch
branchInput = applyLocation $ do
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
     <|> (applyLocation $ do
            reserved "new"
            (v, t) <- parens typedVar
            return $ ANew v (-1) t
         )
     <|> (applyLocation $ do
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
     <|> (applyLocation $ do
           reserved "spawn"
           PCall m k vs _ <- braces call
           return $ ASpawn m k vs
         )
     <|> (applyLocation $ do
           m <- modulQual
           n <- identifier
           args <- parens $ commaSep expr
           return $ APrim m n args
         )
     <?> "action"

output :: Parser Action
output = applyLocation $ do
  c <- identifier
  reservedOp "!"
  e <- expr
  return $ AOutput c e (-1)

input :: Parser Action
input = applyLocation $ do
  c <- identifier
  reservedOp "?"
  x <- parens identifier
  return $ AInput c x (TUnknown noLoc) (-1) (-1)

typeExpr :: Parser TypeExpr
typeExpr = (applyLocation $ do
              reserved "chan"
              t <- angles typeExpr
              return $ TChannel t
           )
       <|> (applyLocation $ parens $ sepBy1 typeExpr (char '*') >>= return . TTuple)
       <|> (applyLocation $ typeAtom >>= return . TAtom)
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
expr = try trueExpr
   <|> try falseExpr
   <|> intExpr
   <|> (applyLocation $ do
          s <- stringLiteral
          return $ \p -> EString (TUnknown noLoc) p s
       )
   <|> (applyLocation $ do
          es <- parens $ commaSep1 expr
          return $ \p -> ETuple (TUnknown noLoc) p es
       )
   <|> varExpr
   <|> (applyLocation $ do
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
trueExpr = applyLocation $ reserved "true" >> return (ETrue (TUnknown noLoc))

falseExpr :: Parser Expr
falseExpr = applyLocation $ reserved "false" >> return (EFalse (TUnknown noLoc))

intExpr :: Parser Expr
intExpr = applyLocation $ do
  i <- fmap fromIntegral integer
  return $ \p -> EInt (TUnknown noLoc) p i

varExpr :: Parser Expr
varExpr = applyLocation $ do
  v <- identifier
  return $ \p -> EVar (TUnknown noLoc) p v (-1)


-- | Module parser
parseModule :: String -> Either PiccError Modul
parseModule s = left (ParsingError . show) $ parse modul "Module Parser" s
