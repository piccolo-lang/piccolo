{-|
Module         : Back.Backend
Description    : Backend interface
Stability      : experimental

This module defines the typeclasses that a backend must be instanciated.
-}
module Back.Backend where

import Back.SeqAST
import Back.CodeEmitter


class BackendTypes a where
  voidType              :: PiccType a
  boolType              :: PiccType a
  intType               :: PiccType a
  stringType            :: PiccType a
  labelType             :: PiccType a
  channelType           :: PiccType a

  defType               :: PiccType a

  ptType                :: PiccType a
  knowSetType           :: PiccType a
  valueType             :: PiccType a
  statusType            :: PiccType a
  tryResultType         :: PiccType a
  incommitType          :: PiccType a
  outcommitType         :: PiccType a
  readyQueueType        :: PiccType a

  schedulerType         :: PiccType a

  stringHandleType      :: PiccType a


class BackendNames a where
  pt                    :: VarDescr a
  ptStatus              :: VarDescr a -> VarDescr a
  ptEnabled             :: VarDescr a -> VarDescr a
  ptKnows               :: VarDescr a -> VarDescr a
  ptEnv                 :: VarDescr a -> VarDescr a
  ptCommit              :: VarDescr a -> VarDescr a
  ptCommits             :: VarDescr a -> VarDescr a
  ptProc                :: VarDescr a -> VarDescr a
  ptPc                  :: VarDescr a -> VarDescr a
  ptVal                 :: VarDescr a -> VarDescr a
  ptClock               :: VarDescr a -> VarDescr a
  ptFuel                :: VarDescr a -> VarDescr a
  ptLock                :: VarDescr a -> VarDescr a

  ptEnvI                :: VarDescr a -> Int -> VarDescr a
  
  chan                  :: [a]
  chanIncommits         :: [a]
  chanOutcommits        :: [a]
  chanGlobalrc          :: [a]
  chanLock              :: [a]
  
  scheduler             :: VarDescr a
  schedulerReady        :: VarDescr a -> VarDescr a
  
  statusRun             :: Value a
  statusCall            :: Value a
  statusWait            :: Value a
  statusBlocked         :: Value a
  statusEnded           :: Value a

  tryResultDisabled     :: Value a
  tryResultEnabled      :: Value a
  tryResultCommit       :: Value a

  ksForgetAll           :: VarDescr a
  kRegister             :: VarDescr a
  
  void                  :: Value a

  makeTrue              :: VarDescr a
  makeFalse             :: VarDescr a
  makeInt               :: VarDescr a
  makeString            :: VarDescr a
  makeStringHandle      :: VarDescr a

  convertInt            :: Int -> Expr a
  convertString         :: String -> Expr a

  processEnd            :: VarDescr a
  processAcquireChannel :: VarDescr a
  processYield          :: VarDescr a
  processWait           :: VarDescr a
  awake                 :: VarDescr a
  generatePiThread      :: VarDescr a
  
  generateChannel       :: VarDescr a
  releaseChannel        :: VarDescr a
  releaseAllChannels    :: VarDescr a
  acquire               :: VarDescr a

  readyQueuePush        :: VarDescr a

  tryOutputAction       :: VarDescr a
  tryInputAction        :: VarDescr a
  registerOutputCommit  :: VarDescr a
  registerInputCommit   :: VarDescr a


class BackendPrimitives a where
  makePrim              :: String -> String -> VarDescr a


-- | The main typeclass of a backend. Contains sequential AST code generation functions.
--class (BackendTypes a, BackendNames a, BackendPrims a) => Backend a where
class (BackendTypes a, BackendNames a, BackendPrimitives a) => Backend a where
  emitVarName   :: VarName a  -> EmitterM ()
  emitPiccType  :: PiccType a -> EmitterM ()
  emitExpr      :: Expr a     -> EmitterM ()
  emitBinop     :: Binop a    -> EmitterM ()
  emitUnop      :: Unop a     -> EmitterM ()
  emitInstr     :: Instr a    -> EmitterM ()
  emitCode      :: String -> Instr a -> EmitterM ()

