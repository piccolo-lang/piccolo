{-|
Module         :
Description    :
Stability      :

Longer description
-}
module PiccError where

import Front.AST

import System.Exit
import Control.Monad.Error

data PiccError
  = SimpleError String
  | ParsingError String
  | TypingError { tErrExpr     :: String
                , tErrLoc      :: Location
                , tErrExpected :: TypeExpr
                , tErrActual   :: TypeExpr
                }
  | TodoError String
                 

instance Error PiccError where
  noMsg  = strMsg ""
  strMsg = SimpleError

instance Show PiccError where
  show (SimpleError str)  = "error: " ++ str
  show (ParsingError str) = "parsing error: " ++ str
  show (TypingError err loc tExp tAct) = "typing error (" ++ show loc ++ "):\n" ++
    "  " ++ err  ++ " is not well-typed,\n" ++
    "  " ++ "Expected type: " ++ show tExp ++ "\n" ++
    "  " ++ "  Actual type: " ++ show tAct
  show (TodoError str) = "TODO: " ++ str

reportResult :: Either PiccError a -> IO a
reportResult (Left err) = do
  putStrLn (show err)
  exitWith (ExitFailure 42)
reportResult (Right result) = return result
