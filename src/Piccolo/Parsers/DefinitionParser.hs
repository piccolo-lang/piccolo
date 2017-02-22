{-|
Module        : Piccolo.Parsers.DefinitionParser
Description   : Piccolo definition parser
Stability     : experimental

These are the parser combinators for parsing piccolo definitions.
-}

module Piccolo.Parsers.DefinitionParser
  ( definition
  , parseDefinition

  )
where

import Piccolo.AST
import Piccolo.Errors
import Piccolo.Parsers.ExpressionParser
import Piccolo.Parsers.Lexer
import Piccolo.Parsers.ProcessParser
import Piccolo.Parsers.Utils

import Control.Arrow
import Text.Parsec hiding (string)
import Text.Parsec.String (Parser)
import Text.Parsec.Language ()


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

-- | Definition parser
parseDefinition :: String -> Either PiccError Definition
parseDefinition s = left (ParsingError . show) $ parse definition "<stdin>" s
