{-# LANGUAGE EmptyDataDecls #-}
module Back.GenericBackend where

import Back.SeqAST
import Back.Backend
import Back.RTOptions

import System.IO

data GenericBackend

instance BackendTypes GenericBackend where
  void                       = undefined
  
  primBool                   = undefined
  primInt                    = undefined
  primString                 = undefined

  ptValue                    = undefined
  ptBool                     = undefined
  ptInt                      = undefined
  ptString                   = undefined
  ptChannel                  = undefined
  ptNoValue                  = undefined

  channel                    = undefined
  handle                     = undefined

  schedPool                  = undefined
  piThread                   = undefined

  mutex                      = undefined
  clock                      = undefined

  commit                     = undefined
  inCommit                   = undefined
  outCommit                  = undefined

  pcLabel                    = undefined
  commitList                 = undefined

  knownSet                   = undefined
  knownValue                 = undefined

  queue                      = undefined
  readyQueue                 = undefined
  waitQueue                  = undefined

  pDef                       = undefined
  evalTy                     = undefined
  evalAsVar                  = undefined

  statusEnum                 = undefined
  statusCall                 = undefined
  statusWait                 = undefined
  statusEnded                = undefined
  statusBlocked              = undefined

  tryEnum                    = undefined
  tryEnabled                 = undefined
  tryDisabled                = undefined
  tryCommit                  = undefined

  commitStatusEnum           = undefined
  commitCannotAcquire        = undefined
  commitValid                = undefined
  commitInvalid              = undefined

  fuelInit                   = undefined
  invalidPC                  = undefined

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
  bnScheduler = error "TODO Back.GenericBackend BackendNames instanciation"
  bnPiThread = error "TODO Back.GenericBackend BackendNames instanciation"

instance Backend GenericBackend where
  emitName = error "TODO Back.GenericBackend Backend instanciation"
  emitVarName = error "TODO Back.GenericBackend Backend instanciation"
  emitPiccType = error "TODO Back.GenericBackend Backend instanciation"
  emitExpr = error "TODO Back.GenericBackend Backend instanciation"
  emitBinop = error "TODO Back.GenericBackend Backend instanciation"
  emitUnop = error "TODO Back.GenericBackend Backend instanciation"
  emitInstr = error "TODO Back.GenericBackend Backend instanciation"
  emitCase = error "TODO Back.GenericBackend Backend instanciation"
  emitCode = error "TODO Back.GenericBackend Backend instanciation"
