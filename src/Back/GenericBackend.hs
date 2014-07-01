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
  ptDef            = error "TODO GenericBackend"

  pt               = error "TODO GenericBackend"
  ptStatus         = error "TODO GenericBackend"
  ptEnabled        = error "TODO GenericBackend"
  ptKnows          = error "TODO GenericBackend"
  ptEnv            = error "TODO GenericBackend"
  ptCommit         = error "TODO GenericBackend"
  ptCommits        = error "TODO GenericBackend"
  ptProc           = error "TODO GenericBackend"
  ptPc             = error "TODO GenericBackend"
  ptVal            = error "TODO GenericBackend"
  ptClock          = error "TODO GenericBackend"
  ptFuel           = error "TODO GenericBackend"
  ptLock           = error "TODO GenericBackend"

  ptEnvI           = error "TODO GenericBackend"

  chan             = error "TODO GenericBackend"
  chanIncommits    = error "TODO GenericBackend"
  chanOutcommits   = error "TODO GenericBackend"
  chanGlobalrc     = error "TODO GenericBackend"
  chanLock         = error "TODO GenericBackend"

  scheduler        = error "TODO GenericBackend"

  labelType        = error "TODO GenericBackend"

  refBool          = error "TODO GenericBackend"
  refInt           = error "TODO GenericBackend"
  refString        = error "TODO GenericBackend"

  defProcType      = error "TODO GenericBackend"

  statusRun        = error "TODO GenericBackend"
  statusCall       = error "TODO GenericBackend"
  statusWait       = error "TODO GenericBackend"
  statusEnded      = error "TODO GenericBackend"

  void             = error "TODO GenericBackend"

  makeTrue         = error "TODO GenericBackend"
  makeFalse        = error "TODO GenericBackend"
  makeInt          = error "TODO GenericBackend"
  makeString       = error "TODO GenericBackend"
  makePrim         = error "TODO GenericBackend"

  convertInt       = error "TODO GenericBackend"
  convertString    = error "TODO GenericBackend"

  processEnd       = error "TODO GenericBackend"


instance Backend GenericBackend where
  emitName = error "TODO Back.GenericBackend Backend instanciation"
  emitVarName = error "TODO Back.GenericBackend Backend instanciation"
  emitPiccType = error "TODO Back.GenericBackend Backend instanciation"
  emitExpr = error "TODO Back.GenericBackend Backend instanciation"
  emitBinop = error "TODO Back.GenericBackend Backend instanciation"
  emitUnop = error "TODO Back.GenericBackend Backend instanciation"
  emitInstr = error "TODO Back.GenericBackend Backend instanciation"
  emitCode = error "TODO Back.GenericBackend Backend instanciation"
