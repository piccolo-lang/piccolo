{-|
Module         : Back.codeEmitter
Description    : Util functions to generate code into a dedicated monad
Stability      : experimental

This module defines the monad EmitterM and functions to use it, to generate code from sequential backend AST.
-}
module Back.CodeEmitter where

import Control.Monad.Writer
import Control.Monad.State


-- | The 'EmitterM' monad carries a state to record indentation level and a writter to output code
type EmitterM a = StateT Int (Writer String) a

-- | The 'runEmitterM' function runs a computation into the 'EmitterM' monad. It takes the monad and return
-- a string representing the output code.
runEmitterM :: EmitterM a -> String
runEmitterM m = execWriter (evalStateT m 0)

-- | 'indSize' is the default number of spaces for an indentation.
indSize :: Int
indSize = 2

-- | 'incrIndent' increments the indentations counter
incrIndent :: EmitterM ()
incrIndent = do
  n <- get
  put (n + indSize)

-- | 'decrIndent' decrements the indentations counter
decrIndent :: EmitterM ()
decrIndent = do
  n <- get
  put (n - indSize)

emitIndent :: EmitterM ()
emitIndent = do
  n <- get
  tell (replicate n ' ')

emitStr :: String -> EmitterM ()
emitStr = lift . tell

emitLn :: String -> EmitterM ()
emitLn str = do
  emitIndent
  emitStr $ str ++ "\n"

emitList :: (a -> EmitterM ()) -> String -> [a] -> EmitterM ()
emitList _ _ []     = return ()
emitList f _ [x]    = f x
emitList f s (x:xs) = do
  f x
  emitStr s
  emitList f s xs

