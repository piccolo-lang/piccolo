{-# LANGUAGE EmptyDataDecls #-}
{-|
Module         : Back.GenericBackend
Description    : A generic backend module
Stability      : experimental

This backend targets pseudo-code, as the one in which compilation schemes of the documentation
are written.
-}
module Back.GenericBackend where

import Back.SeqAST
import Back.Backend

import System.IO

-- | 'GenericBackend' is the empty datatype used in the place of the phantom type in sequential AST.
data GenericBackend

{-instance BackendTypes GenericBackend where
  void                       = Sty "void"
  
  primBool                   = Sty "bool"
  primInt                    = Sty "int"
  primString                 = Sty "string"

  ptValue                    = undefined
  ptBool                     = undefined
  ptInt                      = undefined
  ptString                   = undefined
  ptChannel                  = undefined
  ptNoValue                  = undefined

  channel                    = undefined
  handle                     = undefined

  schedPool                  = Sty "SchedPool"
  piThread                   = Sty "PiThread"

  mutex                      = Sty "Mutex"
  clock                      = Sty "Clock"

  commit                     = Sty "Commit"
  inCommit                   = Sty "InCommit"
  outCommit                  = Sty "OutCommit"

  pcLabel                    = Sty "Label"
  commitList                 = Sty "CommitList"

  knownSet                   = undefined
  knownValue                 = undefined

  queue                      = undefined
  readyQueue                 = undefined
  waitQueue                  = undefined

  pDef                       = undefined
  evalTy                     = undefined
  evalAsVar                  = undefined

  statusEnum                 = Sty "StatusEnum"
  statusRun                  = Val ("StatusRun", statusEnum)
  statusCall                 = Val ("StatusCall", statusEnum)
  statusWait                 = Val ("StatusWait", statusEnum)
  statusEnded                = Val ("StatusEnded", statusEnum)
  statusBlocked              = Val ("StatusBlocked", statusEnum)

  tryEnum                    = Sty "TryResultEnum"
  tryEnabled                 = Val ("TryEnabled", tryEnum)
  tryDisabled                = Val ("TryDisabled", tryEnum)
  tryCommit                  = Val ("TryCommit", tryEnum)

  commitStatusEnum           = Sty "CommitStatusEnum"
  commitCannotAcquire        = Val ("CannotAcquire", commitStatusEnum)
  commitValid                = Val ("ValidCommitment", commitStatusEnum)
  commitInvalid              = Val ("InvalideCommitment", commitStatusEnum)

  fuelInit                   = Val ("FuelInit", primInt)
  invalidPC                  = Val ("InvalidPC", pcLabel)

  makeTrue                   = undefined
  makeFalse                  = undefined
  makeInt                    = undefined
  makeString                 = undefined
  makeChannel                = undefined

  createStringHandle         = undefined

  dEntry                     = undefined

  null                       = undefined
  zero                       = undefined
  primFalse                  = undefined
  primTrue                   = undefined
  pcLabelInit                = undefined
  tryResultInit              = undefined

instance BackendNames GenericBackend where
  copyValue                  = undefined
  boolOfBoolValue            = undefined
  outCommitsOfChannelValue   = undefined
  inCommitsOfChannelValue    = undefined

  evalFunOfOutCommit         = undefined

  awake                      = undefined
  canAwake                   = undefined

  getHandle                  = undefined
  acquireHandle              = undefined
  handleGlobalRC             = undefined

  handleDecRefCount          = undefined
  handleIncRefCount          = undefined

  fetchInputCommitment       = undefined
  fetchOutputCommitment      = undefined
  registerInputCommitment    = undefined
  registerOutputCommitment   = undefined
  commitListIsEmpty          = undefined

  emptyKnownSet              = undefined
  freeKnownSet               = undefined
  knownSetAdd                = undefined
  knownSetRegister           = undefined
  knownSetForgetAll          = undefined
  knownSetForgetToUnknown    = undefined
  knownSetForget             = undefined
  knownSetKnown              = undefined

  waitQueuePush              = undefined
  readyQueuePush             = undefined
  readyQueueAdd              = undefined
  releaseAllChannels         = undefined
  acquire                    = undefined
  release                    = undefined
  lowLevelYield              = undefined

  generateChannel            = undefined
  generatePiThread           = undefined
  scheduler                  = undefined
  schedReady                 = undefined
  schedWait                  = undefined

  pt                         = undefined
  ptStatus                   = undefined
  ptEnabled                  = undefined
  ptKnown                    = undefined
  ptEnv                      = undefined
  ptCommit                   = undefined
  ptCommits                  = undefined
  ptProc                     = undefined
  ptPC                       = undefined
  ptVal                      = undefined
  ptClock                    = undefined
  ptFuel                     = undefined
  ptLock                     = undefined
  ptChans                    = undefined

  tryResult                  = undefined

  chan                       = undefined
  chans                      = undefined

  outCommitVar               = undefined
  outCommitThread            = undefined
  outCommitThreadVal         = undefined

  inCommitVar                = undefined
  inCommitThread             = undefined
  inCommitIn                 = undefined
  inCommitRefVar             = undefined
  inCommitThreadEnvRV        = undefined

  args                       = undefined
  child                      = undefined

  childProc                  = undefined
  childPC                    = undefined
  childStatus                = undefined
  childKnown                 = undefined
  childEnv                   = undefined

instance BackendPrims GenericBackend where
  addName                    = undefined
  substractName              = undefined
  moduloName                 = undefined
  equalsName                 = undefined
  lessThanName               = undefined
  printInfoName              = undefined
  printStrName               = undefined
  printIntName               = undefined-}

instance Backend GenericBackend where
  emitName = error "TODO Back.GenericBackend Backend instanciation"
  emitVarName = error "TODO Back.GenericBackend Backend instanciation"
  emitPiccType = error "TODO Back.GenericBackend Backend instanciation"
  emitExpr = error "TODO Back.GenericBackend Backend instanciation"
  emitBinop = error "TODO Back.GenericBackend Backend instanciation"
  emitUnop = error "TODO Back.GenericBackend Backend instanciation"
  emitInstr = error "TODO Back.GenericBackend Backend instanciation"
  emitCode = error "TODO Back.GenericBackend Backend instanciation"
