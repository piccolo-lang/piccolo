{-|
Module         : Backend.Codegen
Description    : Util functions to generate code into a dedicated monad
Stability      : experimental

This module defines the monad EmitterM and functions to use it,
to generate code from sequential backend AST.
-}
module Piccolo.Codegen
  ( EmitterM
  , runEmitterM
  , incrIndent
  , decrIndent
  , emitStr
  , emitLn
  , emitLn0
  , emitList
  , emitIndent
  )
where

import Control.Monad.Writer
import Control.Monad.State


-- | The 'EmitterM' monad carries a state to record indentation level
-- and a writer to output code
type EmitterM a = StateT Int (Writer String) a

-- | The 'runEmitterM' function runs a computation into the 'EmitterM' monad.
-- It returns the emitted code in a 'String'.
runEmitterM :: EmitterM a -> String
runEmitterM m = execWriter (evalStateT m 0)

-- Default indentation size
indSize :: Int
indSize = 2

-- | 'incrIndent' increments the indentation counter
incrIndent :: EmitterM ()
incrIndent = modify (+ indSize)

-- | 'decrIndent' decrements the indentation counter
decrIndent :: EmitterM ()
decrIndent = modify (subtract indSize)

-- | Emits the current number of indentations
emitIndent :: EmitterM ()
emitIndent = do
  n <- get
  tell (replicate n ' ')

-- | Emits a string
emitStr :: String -> EmitterM ()
emitStr = lift . tell

-- | Emits a line with indentation followed by the specified string
emitLn :: String -> EmitterM ()
emitLn str = do
  emitIndent
  emitStr $ str ++ "\n"

-- | Emits a line with *no* indentation
emitLn0 :: String -> EmitterM ()
emitLn0 str = emitStr $ str ++ "\n"

-- | Emits a list with the specified separator
emitList :: (a -> EmitterM ()) -> String -> [a] -> EmitterM ()
emitList _ _ []     = return ()
emitList f _ [x]    = f x
emitList f s (x:xs) = do
  f x
  emitStr s
  emitList f s xs

