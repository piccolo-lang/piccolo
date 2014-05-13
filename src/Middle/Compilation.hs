{-|
Module         : Middle.Compilation
Description    : Compilation module
Stability      : experimental

This module contains the compilation pass. It transforms a piccolo AST to a sequential AST.
-}
module Middle.Compilation (compilePass) where

import PiccError
import Front.AST
import qualified Back.SeqAST as S
import qualified Back.Backend as B

import Control.Monad.Error
import Control.Monad.Identity

type CompilingM a = ErrorT PiccError Identity a


-- | The 'compilePass' monad run the piccolo AST compilation to sequential AST
compilePass :: (B.Backend a) => ModuleDef -> Either PiccError (S.Instr a)
compilePass mDef = runIdentity $ runErrorT instr
  where instr = compileModule mDef


genFunSig :: (B.Backend a) => String -> Definition -> CompilingM (S.Instr a)
genFunSig m def = return $ S.DeclareFun (S.SimpleName n, B.pDef) [B.scheduler, B.pt] (S.SeqBloc [])
  where n = S.Name (m ++ "_" ++ defName def)

genFunDef :: (B.Backend a) => String -> Definition -> CompilingM (S.Instr a)
genFunDef m def = do
  initLabel
  processInstrs <- compileProcess (defBody def)
  return $ S.DeclareFun (S.SimpleName n, B.pDef) [B.scheduler, B.pt]
                        (S.SeqBloc [ S.Label "undefined"
                                   , S.Switch (S.Var B.ptPC)
                                     (S.SeqBloc [S.Case B.dEntry processInstrs])
                                   ])
  where
    initLabel = undefined
    n = S.Name (m ++ "_" ++ defName def)

compileModule :: (B.Backend a) => ModuleDef -> CompilingM (S.Instr a)
compileModule mDef = do
  defSigs <- mapM (genFunSig m) defs
  defDefs <- mapM (genFunDef m) defs
  return $ S.SeqBloc [ S.SeqBloc evalFuns
                     , S.SeqBloc defSigs
                     , S.SeqBloc defDefs
                     ]
  where
    defs     = moduleDefs mDef
    m        = moduleName mDef
    evalFuns = undefined

compileProcess :: (B.Backend a) => Process -> CompilingM (S.Instr a)
compileProcess proc@(PEnd {})    = compileEnd B.statusEnded
compileProcess proc@(PChoice {}) = throwError $ TodoError "Middle.Compilation.compileProcess PChoice"
compileProcess proc@(PCall {})   = throwError $ TodoError "Middle.Compilation.compileProcess PCall"

compileEnd :: (B.Backend a) => S.Expr a -> CompilingM (S.Instr a)
compileEnd status = do
   return $ S.SeqBloc [ S.ForEach B.chan (S.FunCall B.knownSetKnown [S.Var B.ptKnown])
                        (S.SeqBloc [S.ProcCall B.handleDecRefCount [S.FunCall B.getHandle [S.Var B.chan]]])
                      , S.ForEach B.chan (S.FunCall B.knownSetForget [S.Var B.ptKnown])
                        (S.SeqBloc [S.ProcCall B.handleDecRefCount [S.FunCall B.getHandle [S.Var B.chan]]])
                      , S.Assign B.ptStatus status
                      , S.Return B.void
                      ]
                                            
compileValue :: (B.Backend a) => Value -> CompilingM (S.Instr a)
compileValue val@(VTrue {})   = return $ S.ProcCall B.makeTrue [S.Var B.ptVal]
compileValue val@(VFalse {})  = return $ S.ProcCall B.makeFalse [S.Var B.ptVal]
compileValue val@(VInt {})    = return $ S.ProcCall B.makeInt [S.Var B.ptVal, S.Val (B.makePrimInt (valInt val))]
compileValue val@(VString {}) = throwError $ TodoError "Middle.Compilation.compileValue VString"
compileValue val@(VTuple {})  = throwError $ TodoError "Middle.Compilation.compileValue VTuple"
compileValue val@(VVar {})    = return $ S.Assign B.ptVal (S.Var (B.ptEnv (valIndex val)))
compileValue val@(VPrim {})   = throwError $ TodoError "Middle.Compilation.compileValue VPrim"
