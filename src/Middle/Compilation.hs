{-|
Module         :
Description    :
Stability      :

Longer description
-}
module Middle.Compilation (compilePass) where

import PiccError
import Front.AST
import qualified Back.SeqAST as S
import qualified Back.Backend as B

import Control.Monad.Error
import Control.Monad.Identity


type CompilingM a = ErrorT PiccError Identity a


compilePass :: (B.Backend a) => ModuleDef -> Either PiccError (S.Instr a)
compilePass mDef = runIdentity $ runErrorT instr
  where instr = compileModule mDef


compileModule :: (B.Backend a) => ModuleDef -> CompilingM (S.Instr a)
compileModule mDef = throwError $ TodoError "Middle.Compilation.compileModule"

compileDefinition :: (B.Backend a) => Definition -> CompilingM (S.Instr a)
compileDefinition def = throwError $ TodoError "Middle.Compilation.compileDefinition"

-- compileProcess
--
-- compileBranch
--
-- compileAction

compileValue :: (B.Backend a) => Value -> CompilingM (S.Instr a)
compileValue val@(VTrue {})   = throwError $ TodoError "Middle.Compilation.compileValue VTrue"
compileValue val@(VFalse {})  = throwError $ TodoError "Middle.Compilation.compileValue VFalse"
compileValue val@(VInt {})    = throwError $ TodoError "Middle.Compilation.compileValue VInt"
compileValue val@(VString {}) = throwError $ TodoError "Middle.Compilation.compileValue VString"
compileValue val@(VTuple {})  = throwError $ TodoError "Middle.Compilation.compileValue VTuple"
compileValue val@(VVar {})    = throwError $ TodoError "Middle.Compilation.compileValue VVar"
compileValue val@(VPrim {})   = throwError $ TodoError "Middle.Compilation.compileValue VPrim"
