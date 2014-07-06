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


-- | 'GenericBackend' is the empty datatype used in the place of the phantom type in sequential AST.
data GenericBackend

instance BackendTypes GenericBackend where
  voidType              = error "TODO GenericBackend voidType"
  boolType              = error "TODO GenericBackend boolType"
  intType               = error "TODO GenericBackend intType"
  stringType            = error "TODO GenericBackend stringType"
  labelType             = error "TODO GenericBackend labelType"
  channelType           = error "TODO GenericBackend channelType"

  defType               = error "TODO GenericBackend defType"

  ptType                = error "TODO GenericBackend ptType"
  knowSetType           = error "TODO GenericBackend knowSetType"
  valueType             = error "TODO GenericBackend valueType"
  statusType            = error "TODO GenericBackend statusType"
  tryResultType         = error "TODO GenericBackend tryResultType"
  incommitType          = error "TODO GenericBackend incommitType"
  outcommitType         = error "TODO GenericBackend outcommitType"

  schedulerType         = error "TODO GenericBackend schedulerType"

  stringHandleType      = error "TODO GenericBackend stringHandleType"


instance BackendNames GenericBackend where
  ptDef                 = error "TODO GenericBackend ptDef"

  pt                    = error "TODO GenericBackend pt"
  ptStatus              = error "TODO GenericBackend ptStatus"
  ptEnabled             = error "TODO GenericBackend ptEnabeld"
  ptKnows               = error "TODO GenericBackend ptKnows"
  ptEnv                 = error "TODO GenericBackend ptEnv"
  ptCommit              = error "TODO GenericBackend ptCommit"
  ptCommits             = error "TODO GenericBackend ptCommits"
  ptProc                = error "TODO GenericBackend ptProc"
  ptPc                  = error "TODO GenericBackend ptPc"
  ptVal                 = error "TODO GenericBackend ptVal"
  ptClock               = error "TODO GenericBackend ptClock"
  ptFuel                = error "TODO GenericBackend ptFuel"
  ptLock                = error "TODO GenericBackend ptLock"
  
  ptEnvI                = error "TODO GenericBackend ptEnvI"

  chan                  = error "TODO GenericBackend chan"
  chanIncommits         = error "TODO GenericBackend chanIncommits"
  chanOutcommits        = error "TODO GenericBackend chanOutcommits"
  chanGlobalrc          = error "TODO GenericBackend chanGlobalrc"
  chanLock              = error "TODO GenericBackend chanLock"

  scheduler             = error "TODO GenericBackend scheduler"

  statusRun             = error "TODO GenericBackend statusRun"
  statusCall            = error "TODO GenericBackend statusCall"
  statusWait            = error "TODO GenericBackend statusWait"
  statusBlocked         = error "TODO GenericBackend statusBlocked"
  statusEnded           = error "TODO GenericBackend statusEnded"

  tryResultDisabled     = error "TODO GenericBackend tryResultDisabled"
  tryResultEnabled      = error "TODO GenericBackend tryResultEnabled"
  tryResultCommit       = error "TODO GenericBackend tryResultCommit"

  ksForgetAll           = error "TODO GenericBackend ksForgetAll"
  kRegister             = error "TODO GenericBackend kRegister"

  void                  = error "TODO GenericBackend void"

  makeTrue              = error "TODO GenericBackend makeTrue"
  makeFalse             = error "TODO GenericBackend makeFalse"
  makeInt               = error "TODO GenericBackend makeInt"
  makeString            = error "TODO GenericBackend makeString"
  makeStringHandle      = error "TODO GenericBackend makeStringHandle"

  convertInt            = error "TODO GenericBackend convertInt"
  convertString         = error "TODO GenericBackend convertString"

  processEnd            = error "TODO GenericBackend processEnd"
  processAcquireChannel = error "TODO GenericBackend processAcquireChannel"
  processYield          = error "TODO GenericBackend processYield"
  processWait           = error "TODO GenericBackend processWait"
  awake                 = error "TODO GenericBackend awake"
  generatePiThread      = error "TODO GenericBackend generatePiThread"

  generateChannel       = error "TODO GenericBackend generateChannel"
  releaseChannel        = error "TODO GenericBackend releaseChannel"
  releaseAllChannels    = error "TODO GenericBackend releaseAllChannels"
  acquire               = error "TODO GenericBackend acquire"

  readyQueuePush        = error "TODO GenericBackend readyQueuePush"

  tryOutputAction       = error "TODO GenericBackend tryOutputAction"
  tryInputAction        = error "TODO GenericBackend tryInputAction"
  registerOutputCommit  = error "TODO GenericBackend registerOutputCommit"
  registerInputCommit   = error "TODO GenericBackend registerInputCommit"


instance BackendPrimitives GenericBackend where
  makePrim              = error "TODO GenericBackend makePrim"


instance Backend GenericBackend where
  emitVarName = error "TODO GenericBackend.emitVarName"
  emitPiccType = error "TODO GenericBackend.emitPiccType"
  emitExpr = error "TODO GenericBackend.emitExpr"
  emitBinop = error "TODO GenericBackend.emitBinop"
  emitUnop = error "TODO GenericBackend.emitUnop"
  emitInstr = error "TODO GenericBackend.emitInstr"
  emitCode = error "TODO GenericBackend.emitCode"

