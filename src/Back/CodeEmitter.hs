{-|
Module         :
Description    :
Stability      :

Longer description
-}
module Back.CodeEmitter where

import Control.Monad.Writer
import Control.Monad.State

type EmitterM a = StateT Int (Writer String) a

runEmitterM :: EmitterM a -> String
runEmitterM m = execWriter (evalStateT m 0)

incrSize :: Int
incrSize = 4

incrIndent :: EmitterM ()
incrIndent = do
  n <- get
  put (n + incrSize)

decrIndent :: EmitterM ()
decrIndent = do
  n <- get
  put (n - incrSize)
