{-|
Module         : Errors
Description    : Errors handling module
Stability      : experimental

This module defines the various types of errors that can occur during
the compilation of a piccolo program.
-}
module Errors
  ( PiccError (..)
  , reportResult
  )
where

import Core.AST

import System.Exit
import Control.Monad.Error


-- | The 'PiccError' type separate compilation errors into several categories.
data PiccError
  = SimpleError String
    -- ^ error that does not fit into other categories
  | VarNotFoundError String Location
    -- ^ looking for a variable and not finding it
  | DefNotFoundError String Location
    -- ^ looking for a process definition and not finding it
  | PrimNotFoundError String Location
    -- ^ looking for a primitive definition and not finding it
  | ArityError
    { aErrName     :: String
    , aErrLoc      :: Location
    , aErrExpected :: Int
    , aErrActual   :: Int
    }
    -- ^ error occuring when calling a def with a bad arity
  | ParsingError String
    -- ^ error occuring during the parsing of a piccolo program
  | TypingError
    { tErrExpr     :: String
    , tErrLoc      :: Location
    , tErrExpected :: TypeExpr
    , tErrActual   :: TypeExpr
    }
    -- ^ error occuring during the typing pass of a piccolo AST

instance Error PiccError where
  noMsg  = strMsg ""
  strMsg = SimpleError

instance Show PiccError where
  show (SimpleError str) =
    "error: " ++ str

  show (VarNotFoundError var loc) =
    "error (" ++ show loc ++ "): variable '" ++ var ++ "' not found"

  show (DefNotFoundError def loc) =
    "error (" ++ show loc ++ "): definition '" ++ def ++ "' not found"

  show (PrimNotFoundError prim loc) =
    "error (" ++ show loc ++ "): definition '" ++ prim ++ "' not found"

  show (ArityError n loc aExp aAct) =
    "arity error (" ++ show loc ++ "):\n" ++
    "  '" ++ n ++ "' is called with bad arity,\n" ++
    "  " ++ "Expected arity: " ++ show aExp ++ "\n" ++
    "  " ++ "  Actual arity: " ++ show aAct

  show (ParsingError str) =
    "parsing error: " ++ str

  show (TypingError err loc tExp tAct) =
    "typing error (" ++ show loc ++ "):\n" ++
    "  '" ++ err  ++ "' is not well-typed,\n" ++
    "  " ++ "Expected type: " ++ show tExp ++ "\n" ++
    "  " ++ "  Actual type: " ++ show tAct

-- | The 'reportResult' function takes a result (of 'Either' type) and
-- return it into the IO  monad, or print the error and quit the program.
reportResult :: Either PiccError a -> IO a
reportResult (Left err) = do
  print err
  exitWith (ExitFailure 42)
reportResult (Right result) = return result
