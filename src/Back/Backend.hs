{-|
Module         : Back.Backend
Description    : Backend interface
Stability      : experimental

This module defines the typeclasses that a backend must be instanciated.
-}
module Back.Backend where

import Back.SeqAST
import Back.RTOptions
import Back.CodeEmitter


-- | The main typeclass of a backend. Contains sequential AST code generation functions.
class (BackendTypes a, BackendNames a, BackendPrims a) => Backend a where
  emitName      :: Name a     -> EmitterM ()
  emitVarName   :: VarName a  -> EmitterM ()
  emitPiccType  :: PiccType a -> EmitterM ()
  emitExpr      :: Expr a     -> EmitterM ()
  emitBinop     :: Binop a    -> EmitterM ()
  emitUnop      :: Unop a     -> EmitterM ()
  emitInstr     :: Instr a    -> EmitterM ()
  emitCase      :: Case a     -> EmitterM ()
  emitCode      :: RTOptions -> String -> Instr a -> EmitterM ()

-- | The 'BackendTypes' class forces to specify the various types used by the runtime.
class BackendTypes a where
  void                       :: PiccType a

  -- primitive types (of the target language)
  primBool                   :: PiccType a
  primInt                    :: PiccType a
  primString                 :: PiccType a
  
  -- types of the runtime library
  ptValue                    :: PiccType a
  ptBool                     :: PiccType a
  ptInt                      :: PiccType a
  ptString                   :: PiccType a
  ptChannel                  :: PiccType a
  ptNoValue                  :: PiccType a

  channel                    :: PiccType a
  handle                     :: PiccType a

  schedPool                  :: PiccType a
  piThread                   :: PiccType a

  mutex                      :: PiccType a
  clock                      :: PiccType a

  commit                     :: PiccType a
  inCommit                   :: PiccType a
  outCommit                  :: PiccType a

  pcLabel                    :: PiccType a
  commitList                 :: PiccType a

  knownSet                   :: PiccType a
  knownValue                 :: PiccType a

  queue                      :: PiccType a
  readyQueue                 :: PiccType a
  waitQueue                  :: PiccType a

  pDef                       :: PiccType a
  evalTy                     :: PiccType a
  evalAsVar                  :: VarDescr a

  -- enum type for status and corresponding values
  statusEnum                 :: PiccType a
  statusRun                  :: Expr a
  statusCall                 :: Expr a
  statusWait                 :: Expr a
  statusEnded                :: Expr a
  statusBlocked              :: Expr a

  -- enum type for try and corresponding values
  tryEnum                    :: PiccType a
  tryEnabled                 :: Expr a
  tryDisabled                :: Expr a
  tryCommit                  :: Expr a

  -- enum type for commit status nd corresponding values
  commitStatusEnum           :: PiccType a
  commitCannotAcquire        :: Expr a
  commitValid                :: Expr a
  commitInvalid              :: Expr a

  -- constant values
  fuelInit                   :: Expr a
  invalidPC                  :: Expr a

  -- value initialization functions
  makeTrue                   :: VarDescr a
  makeFalse                  :: VarDescr a
  makeInt                    :: VarDescr a
  makeString                 :: VarDescr a
  makeChannel                :: VarDescr a

  -- string allocation function
  createStringHandle         :: String -> Expr a

  -- entry point
  dEntry                     :: Expr a
  
  -- some misc values
  null                       :: Value a
  zero                       :: Value a
  primFalse                  :: Value a
  primTrue                   :: Value a
  pcLabelInit                :: Value a
  tryResultInit              :: Expr a


-- | The 'BackendNames' class forces to specify the various names used by a runtime.
class BackendNames a where
  copyValue                  :: Name a
  boolOfBoolValue            :: Name a
  outCommitsOfChannelValue   :: Name a
  inCommitsOfChannelValue    :: Name a
  
  evalFunOfOutCommit         :: Name a

  awake                      :: Name a
  canAwake                   :: Name a

  getHandle                  :: Name a
  acquireHandle              :: Name a
  handleGlobalRC             :: Name a

  handleDecRefCount          :: Name a
  handleIncRefCount          :: Name a

  fetchInputCommitment       :: Name a
  fetchOutputCommitment      :: Name a
  registerInputCommitment    :: Name a
  registerOutputCommitment   :: Name a
  commitListIsEmpty          :: Name a

  emptyKnownSet              :: Name a
  freeKnownSet               :: Name a
  knownSetAdd                :: Name a
  knownSetRegister           :: Name a
  knownSetForgetAll          :: Name a
  knownSetForgetToUnknown    :: Name a
  knownSetForget             :: Name a
  knownSetKnown              :: Name a

  -- thread synchronization functions
  waitQueuePush              :: Name a
  readyQueuePush             :: Name a
  readyQueueAdd              :: Name a
  releaseAllChannels         :: Name a
  acquire                    :: Name a
  release                    :: Name a
  lowLevelYield              :: Name a

  generateChannel            :: Name a
  generatePiThread           :: Name a

  -- schedpool fields
  scheduler                  :: Name a
  schedReady                 :: Name a
  schedWait                  :: Name a

  -- pithread fields
  pt                         :: Name a
  ptStatus                   :: Name a
  ptEnabled                  :: Name a
  ptKnown                    :: Name a
  ptEnv                      :: Name a
  ptCommit                   :: Name a
  ptCommits                  :: Name a
  ptProc                     :: Name a
  ptPC                       :: Name a
  ptVal                      :: Name a
  ptClock                    :: Name a
  ptFuel                     :: Name a
  ptLock                     :: Name a
  ptChans                    :: Name a

  tryResult                  :: Name a

  chan                       :: Name a
  chans                      :: Name a

  outCommitVar               :: Name a
  outCommitThread            :: Name a
  outCommitThreadVal         :: Name a

  inCommitVar                :: Name a
  inCommitThread             :: Name a
  inCommitIn                 :: Name a
  inCommitRefVar             :: Name a
  inCommitThreadEnvRV        :: Name a

  args                       :: Name a
  child                      :: Name a

  childProc                  :: Name a
  childPC                    :: Name a
  childStatus                :: Name a
  childKnown                 :: Name a
  childEnv                   :: Name a


-- | The 'BackendPrims' class forces to specify the various primitive names used by a runtime.
class BackendPrims a where
  addName                    :: Name a
  substractName              :: Name a
  moduloName                 :: Name a
  equalsName                 :: Name a
  lessThanName               :: Name a
  printInfoName              :: Name a
  printStrName               :: Name a
  printIntName               :: Name a

