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
  ptStatus         :: [a]
  ptEnabled        :: [a]
  ptKnows          :: [a]
  ptEnv            :: VarDescr a
  ptCommit         :: [a]
  ptCommits        :: [a]
  ptProc           :: [a]
  ptPc             :: VarDescr a
  ptVal            :: VarDescr a
  ptClock          :: [a]
  ptFuel           :: [a]
  ptLock           :: [a]

  ptEnvI           :: Int -> VarDescr a
  
  chan             :: [a]
  chanIncommits    :: [a]
  chanOutcommits   :: [a]
  chanGlobalrc     :: [a]
  chanLock         :: [a]
  
  scheduler        :: VarDescr a
  
  labelType        :: PiccType a
  
  refBool          :: PiccType a
  refInt           :: PiccType a
  refString        :: PiccType a

  defProcType      :: PiccType a

  statusRun        :: Value a
  statusCall       :: Value a
  statusWait       :: Value a
  statusEnded      :: Value a
  
  void             :: Value a

  makeTrue         :: VarDescr a
  makeFalse        :: VarDescr a
  makeInt          :: VarDescr a
  makeString       :: VarDescr a
  makePrim         :: String -> String -> VarDescr a

  convertInt       :: Int -> Value a
  convertString    :: String -> Value a

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

