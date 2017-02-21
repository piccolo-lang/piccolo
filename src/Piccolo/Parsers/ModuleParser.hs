{-|
Module        : Core.Parser
Description   : Piccolo-core parser
Stability     : experimental

These are the parser combinators for parsing piccolo-core modules.
-}

module Piccolo.Parsers.ModuleParser
  ( parseModule
  )
where

import Piccolo.AST
import Piccolo.Errors
import Piccolo.Parsers.DefinitionParser
import Piccolo.Parsers.Lexer
import Piccolo.Parsers.Utils

import Control.Arrow
import Text.Parsec hiding (string)
import Text.Parsec.String (Parser)
import Text.Parsec.Language ()



modul :: Parser Modul
modul = do
  whiteSpace
  m <- withLocation $ do
    reserved "module"
    fullName <- modulId
    defs <- many definition
    return $ Modul fullName defs
  eof
  return m

-- | Module parser
parseModule :: String -> Either PiccError Modul
parseModule s = left (ParsingError . show) $ parse modul "Module Parser" s
