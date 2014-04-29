module Middle.Typing where

import Front.AST
import Front.ASTUtils

import Control.Monad.Error
import Control.Monad.State
import qualified Data.Map as Map

data TypingError = TypingError { tErrExpr :: String
                               , tErrLoc  :: Location
                               , tErrExpected :: TypeExpr
                               , tErrFound    :: TypeExpr
                               }

instance Error TypingError where
  noMsg  = undefined
  strMsg = undefined

type TypingDefsEnv = [Definition]
type TypingVarsEnv = Map.Map String TypeExpr

type TypingEnv = (TypingDefsEnv, TypingVarsEnv)

type TypingM a = ErrorT TypingError (State TypingEnv) a

typingPass :: ModuleDef -> ModuleDef
typingPass mDef = case evalState (runErrorT tChecked) initEnv of
  Left tErr  -> error "typing error!"
  Right tAst -> tAst
  where tChecked = tcModule mDef
        initEnv  = (moduleDefs mDef, Map.empty)

tcModule :: ModuleDef -> TypingM ModuleDef
tcModule mDef = do
  defs <- mapM tcDefinition $ moduleDefs mDef
  return $ mDef { moduleDefs = defs }

tcDefinition :: Definition -> TypingM Definition
tcDefinition = error "TODO Middle.Typing.tcDefinition"
