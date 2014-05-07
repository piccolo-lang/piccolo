{-|
Module         :
Description    :
Stability      :

Longer description
-}
{-# LANGUAGE EmptyDataDecls #-}
module Back.CBackend where

import Back.SeqAST
import Back.Backend
import Back.RTOptions

import System.IO

data CBackend

pointer :: (BackendTypes a) => PiccType a -> PiccType a
pointer t = Pty "*" t

instance BackendTypes CBackend where
  void                       = Sty "void"
  
  primBool                   = Sty "bool"
  primInt                    = Sty "int"
  primString                 = pointer $ Sty "char"

  ptValue                    = Sty "PICC_Value"
  ptBool                     = pointer $ Sty "PICC_BoolValue"
  ptInt                      = pointer $ Sty "PICC_IntValue"
  ptString                   = pointer $ Sty "PICC_StringValue"
  ptChannel                  = pointer $ Sty "PICC_ChannelValue"
  ptNoValue                  = pointer $ Sty "PICC_NoValue"

  channel                    = pointer $ Sty "PICC_Channel"
  handle                     = pointer $ Sty "PICC_Handle"

  schedPool                  = pointer $ Sty "PICC_SchedPool"
  piThread                   = pointer $ Sty "PICC_PiThread"

  mutex                      = pointer $ Sty "PICC_Mutex"
  clock                      = Sty "PICC_Clock"

  commit                     = pointer $ Sty "PICC_Commit"
  inCommit                   = pointer $ Sty "PICC_InCommit"
  outCommit                  = pointer $ Sty "PICC_OutCommit"

  pcLabel                    = Sty "PICC_Label"
  commitList                 = Sty "PICC_CommitList"

  knownSet                   = pointer $ Sty "PICC_KnownSet"
  knownValue                 = pointer $ Sty "PICC_KnownValue"

  queue                      = Sty "PICC_Queue"
  readyQueue                 = pointer $ Sty "PICC_ReadyQueue"
  waitQueue                  = pointer $ Sty "PICC_WaitQueue"

  pDef                       = Fun void [schedPool, piThread]
  evalTy                     = Fun ptValue [piThread]
  evalAsVar                  = (SimpleName (Name "evalFunc"), Sty "PICC_EvalFunction")

  statusEnum                 = Sty "PICC_StatusKind"
  statusCall                 = Val ("PICC_STATUS_CALL", statusEnum)
  statusRun                  = Val ("PICC_STATUS_RUN", statusEnum)
  statusWait                 = Val ("PICC_STATUS_WAIT", statusEnum)
  statusEnded                = Val ("PICC_STATUS_ENDED", statusEnum)
  statusBlocked              = Val ("PICC_STATUS_BLOCKED", statusEnum)

  tryEnum                    = Sty "PICC_TryResult"
  tryEnabled                 = Val ("PICC_TRY_ENABLED", tryEnum)
  tryDisabled                = Val ("PICC_TRY_DISABLED", tryEnum)
  tryCommit                  = Val ("PICC_TRY_COMMIT", tryEnum)

  commitStatusEnum           = Sty "PICC_CommitStatus"
  commitCannotAcquire        = Val ("PICC_CANNOT_ACQUIRE", commitStatusEnum)
  commitValid                = Val ("PICC_VALID_COMMIT", commitStatusEnum)
  commitInvalid              = Val ("PICC_INVALID_COMMIT", commitStatusEnum)

  fuelInit                   = Val ("PICC_FUEL_INIT", primInt)
  invalidPC                  = Val ("PICC_INVALID_PC", pcLabel)

  makeTrue                   = (SimpleName (Name "PICC_INIT_BOOL_TRUE"), Fun void [ptValue])
  makeFalse                  = (SimpleName (Name "PICC_INIT_BOOL_FALSE"), Fun void [ptValue])
  makeInt                    = (SimpleName (Name "PICC_INIT_INT_VALUE"), Fun void [ptValue, primInt])
  makeString                 = (SimpleName (Name "PICC_INIT_STRING_VALUE"), Fun void [ptValue, primString])
  makeChannel                = (SimpleName (Name "PICC_INIT_CHANNEL_VALUE"), Fun void [ptValue, channel])

  createStringHandle str     = FunCall makeStrHndl [Val (str, primString)]
                               where makeStrHndl = (SimpleName (Name "PICC_create_string_handle"), Fun handle [primString])

  dEntry                     = Val ("0", pcLabel)

  null                       = ("NULL", Sty "NULL")
  zero                       = ("0", primInt)
  primFalse                  = ("false", primBool)
  primTrue                   = ("true", primBool)
  pcLabelInit                = ("0", pcLabel)
  tryResultInit              = tryDisabled

instance BackendNames CBackend where
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

instance BackendPrims CBackend where
  addName                    = undefined
  substractName              = undefined
  moduloName                 = undefined
  equalsName                 = undefined
  lessThanName               = undefined
  printInfoName              = undefined
  printStrName               = undefined
  printIntName               = undefined

instance Backend CBackend where
  emitName = error "TODO Back.CBackend Backend instanciation"
  emitVarName = error "TODO Back.CBackend Backend instanciation"
  emitPiccType = error "TODO Back.CBackend Backend instanciation"
  emitExpr = error "TODO Back.CBackend Backend instanciation"
  emitBinop = error "TODO Back.CBackend Backend instanciation"
  emitUnop = error "TODO Back.CBackend Backend instanciation"
  emitInstr = error "TODO Back.CBackend Backend instanciation"
  emitCase = error "TODO Back.CBackend Backend instanciation"
  emitCode = error "TODO Back.CBackend Backend instanciation"
