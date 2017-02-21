{-|
Module        : Piccolo.Parsers.Utils
Description   : Utilities for parsing piccolo
Stability     : experimental

Utilities for parsing piccolo elements.
-}

module Piccolo.Parsers.Utils
  ( withLocation
  , modulId
  , modulQual
  )
where

import Piccolo.AST
import Piccolo.Parsers.Lexer

import Text.Parsec hiding (string)
import Text.Parsec.String (Parser)
import Text.Parsec.Language ()


withLocation :: Parser (Location -> a) -> Parser a
withLocation parser = do
  pos  <- getPosition
  x    <- parser
  pos' <- getPosition
  return $ x Location { locStartLine   = sourceLine pos
                      , locStartColumn = sourceColumn pos
                      , locEndLine     = sourceLine pos'
                      , locEndColumn   = sourceColumn pos'
                      }

modulId :: Parser ModuleName
modulId = ModuleName <$> sepBy1 identifier (reservedOp ".")

modulQual :: Parser ModuleName
modulQual = do
  reservedOp "#"
  m <- modulId
  reservedOp ":"
  return m
