{-|
Module        : Piccolo.Parsers.Utils
Description   : Utilities for parsing combinators
Stability     : experimental

Utilities for parsing combinators.
-}

module Piccolo.Parsers.Utils
  ( -- * Lexing
    lexer
  , identifier
  , reserved
  , reservedOp
  , parens
  , angles
  , braces
  , brackets
  , whiteSpace
  , commaSep
  , commaSep1
  , integer
  , stringLiteral
    -- * Source locations
  , mkLoc
  , withLocation
    -- * Modules naming
  , modulId
  , modulQual
  )
where

import Piccolo.AST

import Text.Parsec hiding (string)
import Text.Parsec.String (Parser)
import Text.Parsec.Language ()
import qualified Text.Parsec.Token as Tok


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
  , Tok.reservedOpNames = [ ",", "?", "!", ".", "=", ":", "#" ]
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

withLocation :: Parser (Location -> a) -> Parser a
withLocation parser = do
  pos <- getPosition
  x <- parser
  pos' <- getPosition
  return $ x (mkLoc pos pos')

modulId :: Parser ModuleName
modulId = ModuleName <$> sepBy1 identifier (reservedOp ".")

modulQual :: Parser ModuleName
modulQual = do
  reservedOp "#"
  m <- modulId
  reservedOp ":"
  return m

