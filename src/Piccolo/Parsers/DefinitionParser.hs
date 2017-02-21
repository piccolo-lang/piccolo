{-|
Module        : Core.Parser
Description   : Piccolo-core parser
Stability     : experimental

These are the parser combinators for parsing piccolo-core modules.
-}

module Piccolo.Parsers.DefinitionParser
  ( definition
  , parseDefinition

  )
where

import Piccolo.Errors
import Piccolo.AST
import Piccolo.Parsers.ExpressionParser
import Piccolo.Parsers.ProcessParser
import Piccolo.Parsers.Lexer

import Text.Parsec hiding (string)
import Text.Parsec.String (Parser)
import Text.Parsec.Language ()

import Control.Arrow


definition :: Parser Definition
definition = withLocation $ do
  reserved "def"
  name   <- identifier
  params <- parens $ commaSep param
  reserved "="
  body   <- process
  return $ Definition name params body (-1) (-1)

param :: Parser (String, TypeExpr, Location)
param = withLocation $ do
  x <- identifier
  reservedOp ":"
  t <- typeExpr
  return $ \p -> (x, t, p)

parseDefinition :: String -> Either PiccError Definition
parseDefinition s = left (ParsingError . show) $ parse definition "Definition Parser" s
