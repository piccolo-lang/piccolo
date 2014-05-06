module Middle.Compilation (compilePass) where

import PiccError
import Front.AST
import Back.SeqAST
import qualified Back.Backend as B

import Control.Monad.Error
import Control.Monad.Identity


type CompilingM a = ErrorT PiccError Identity a


compilePass :: (Backend a) => ModuleDef -> Either PiccError (Instr a)
compilePass mDef = runIdentity $ runErrorT instr
  where instr = compileModule mDef


compileModule :: (Backend a) => ModuleDef -> CompilingM (Instr a)
compileModule = throwError $ TodoError "Middle.Compilation.compileModule"

compileDefinition :: (Backend a) => Definition -> CompilingM Process
compileDefinition = throwError $ TodoError "Middle.Compilation.compileDefinition"

-- compileProcess
--
-- compileBranch
--
-- compileAction

compileValue :: (Backend a) => Value -> CompilingM (Instr a)
compileValue val@(VTrue {})   = ProcCall B.makeTrue [Var B.ptVal]
compileValue val@(VFalse {})  = ProcCall B.makeFalse [Var B.ptVal]
compileValue val@(VInt {})    = ProcCall B.makeInt [Var B.ptVal, Val (B.makePrimInt (valInt val))]
compileValue val@(VString {}) = ProcCall B.makeString [Var ptVal, B.createStringHandle (valStr val)]
compileValue val@(VTuple {})  = throwError $ TodoError "Middle.Compilation.compileValue VTuple"
compileValue val@(VVar {})    = Assign B.ptVal (Var (B.ptEnv (valIndex val)))
compileValue val@(VPrim {})   = throwError $ TodoError "Middle.Compilation.compileValue VPrim"
