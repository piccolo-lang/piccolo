module Middle.Typing where

import Front.AST

import Control.Monad.Error
import Control.Monad.State

data TypingError a = TypingError a TypeExpr TypeExpr

type TypingEnv = String

type TypingM b a = ErrorT (TypingError b) (State TypingEnv) a

typingPass :: ModuleDef -> ModuleDef
typingPass mDef = error "TODO Middle.Typing.typingPass"
