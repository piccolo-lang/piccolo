module PiccError where

import System.Exit
import Control.Monad.Error

data PiccError
  = SimpleError String
  | ParsingError String
  | TypingError

instance Error PiccError where
  noMsg  = strMsg ""
  strMsg = SimpleError

instance Show PiccError where
  show (ParsingError str) = "parsing error: " ++ str
  show TypingError = "typing error"

reportResult :: Either PiccError a -> IO a
reportResult (Left err) = do
  putStrLn (show err)
  exitWith (ExitFailure 42)
reportResult (Right result) = return result
