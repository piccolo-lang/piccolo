{-# LANGUAGE EmptyDataDecls #-}
module Back.GenericBackend where

import Back.SeqAST
import Back.Backend
import Back.RTOptions
import Utils.CodeEmitter

import System.IO

data GenericBackend

instance BackendTypes GenericBackend where
  btProcDef = error "TODO Back.GenericBackend BackendTypes instanciation"

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
