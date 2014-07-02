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

instance BackendConsts GenericBackend where
  ptDef            = error "TODO GenericBackend ptDef"

  pt               = error "TODO GenericBackend pt"
  ptStatus         = error "TODO GenericBackend ptStatus"
  ptEnabled        = error "TODO GenericBackend ptEnabeld"
  ptKnows          = error "TODO GenericBackend ptKnows"
  ptEnv            = error "TODO GenericBackend ptEnv"
  ptCommit         = error "TODO GenericBackend ptCommit"
  ptCommits        = error "TODO GenericBackend ptCommits"
  ptProc           = error "TODO GenericBackend ptProc"
  ptPc             = error "TODO GenericBackend ptPc"
  ptVal            = error "TODO GenericBackend ptVal"
  ptClock          = error "TODO GenericBackend ptClock"
  ptFuel           = error "TODO GenericBackend ptFuel"
  ptLock           = error "TODO GenericBackend ptLock"

  ptEnvI           = error "TODO GenericBackend ptEnvI"

  chan             = error "TODO GenericBackend chan"
  chanIncommits    = error "TODO GenericBackend chanIncommits"
  chanOutcommits   = error "TODO GenericBackend chanOutcommits"
  chanGlobalrc     = error "TODO GenericBackend chanGlobalrc"
  chanLock         = error "TODO GenericBackend chanLock"

  scheduler        = error "TODO GenericBackend scheduler"

  labelType        = error "TODO GenericBackend labelType"
  valueType        = error "TODO GenericBackend valueType"
  stringHandleType = error "TODO GenericBackend stringHandleType"

  statusType       = error "TODO GenericBackend statusType"
  statusRun        = error "TODO GenericBackend statusRun"
  statusCall       = error "TODO GenericBackend statusCall"
  statusWait       = error "TODO GenericBackend statusWait"
  statusBlocked    = error "TODO GenericBackend statusBlocked"
  statusEnded      = error "TODO GenericBackend statusEnded"

  void             = error "TODO GenericBackend void"

  makeTrue         = error "TODO GenericBackend makeTrue"
  makeFalse        = error "TODO GenericBackend makeFalse"
  makeInt          = error "TODO GenericBackend makeInt"
  makeString       = error "TODO GenericBackend makeString"
  makeStringHandle = error "TODO GenericBackend makeStringHandle"
  makePrim         = error "TODO GenericBackend makePrim"

  convertInt       = error "TODO GenericBackend convertInt"
  convertString    = error "TODO GenericBackend convertString"

  processEnd       = error "TODO GenericBackend processEnd"


instance Backend GenericBackend where
  emitName = error "TODO GenericBackend.emitName"
  emitVarName = error "TODO GenericBackend.emitVarName"
  emitPiccType = error "TODO GenericBackend.emitPiccType"
  emitExpr = error "TODO GenericBackend.emitExpr"
  emitBinop = error "TODO GenericBackend.emitBinop"
  emitUnop = error "TODO GenericBackend.emitUnop"
  emitInstr = error "TODO GenericBackend.emitInstr"
  emitCode = error "TODO GenericBackend.emitCode"
