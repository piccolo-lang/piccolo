{-|
Module         : Back.Backend
Description    : Backend interface
Stability      : experimental

This module defines the typeclasses that a backend must be instanciated.
-}
module Back.Backend where

import Back.SeqAST
import Back.CodeEmitter

class BackendConsts a where
  ptDef            :: [a]

  pt               :: VarDescr a
  ptStatus         :: VarDescr a
  ptEnabled        :: VarDescr a
  ptKnows          :: VarDescr a
  ptEnv            :: VarDescr a
  ptCommit         :: VarDescr a
  ptCommits        :: VarDescr a
  ptProc           :: VarDescr a
  ptPc             :: VarDescr a
  ptVal            :: VarDescr a
  ptClock          :: VarDescr a
  ptFuel           :: VarDescr a
  ptLock           :: VarDescr a

  ptEnvI           :: Int -> VarDescr a
  
  chan             :: [a]
  chanIncommits    :: [a]
  chanOutcommits   :: [a]
  chanGlobalrc     :: [a]
  chanLock         :: [a]
  
  scheduler        :: VarDescr a
  
  labelType        :: PiccType a
  valueType        :: PiccType a
  stringHandleType :: PiccType a

  statusType       :: PiccType a
  statusRun        :: Value a
  statusCall       :: Value a
  statusWait       :: Value a
  statusBlocked    :: Value a
  statusEnded      :: Value a
  
  void             :: Value a

  makeTrue         :: VarDescr a
  makeFalse        :: VarDescr a
  makeInt          :: VarDescr a
  makeString       :: VarDescr a
  makeStringHandle :: VarDescr a
  makePrim         :: String -> String -> VarDescr a

  convertInt       :: Int -> Expr a
  convertString    :: String -> Expr a

  processEnd       :: VarDescr a

-- | The main typeclass of a backend. Contains sequential AST code generation functions.
--class (BackendTypes a, BackendNames a, BackendPrims a) => Backend a where
class (BackendConsts a) => Backend a where
  emitName      :: Name a     -> EmitterM ()
  emitVarName   :: VarName a  -> EmitterM ()
  emitPiccType  :: PiccType a -> EmitterM ()
  emitExpr      :: Expr a     -> EmitterM ()
  emitBinop     :: Binop a    -> EmitterM ()
  emitUnop      :: Unop a     -> EmitterM ()
  emitInstr     :: Instr a    -> EmitterM ()
  emitCode      :: String -> Instr a -> EmitterM ()

