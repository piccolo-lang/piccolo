module Middle.Typing (typingPass) where

import Front.AST
import Front.ASTUtils
import PiccError

import Control.Monad.Error
import Control.Monad.State
import qualified Data.Map as Map

type TypingDefsEnv = [Definition]
type TypingVarsEnv = Map.Map String TypeExpr
type TypingEnv = (TypingDefsEnv, TypingVarsEnv)

type TypingM a = ErrorT PiccError (State TypingEnv) a

typingPass :: ModuleDef -> Either PiccError ModuleDef
typingPass mDef = evalState (runErrorT tChecked) initEnv
  where initEnv  = (moduleDefs mDef, Map.empty)
        tChecked = tcModule mDef

tcModule :: ModuleDef -> TypingM ModuleDef
tcModule mDef = do
  defs <- mapM tcDefinition $ moduleDefs mDef
  return $ mDef { moduleDefs = defs }

tcDefinition :: Definition -> TypingM Definition
tcDefinition = error "TODO Middle.Typing.tcDefinition"

