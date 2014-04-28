{-# LANGUAGE EmptyDataDecls #-}
module Back.CBackend where

import Back.SeqAST
import Back.Backend
import Back.RTOptions
import Utils.CodeEmitter

import System.IO

data CBackend

instance BackendTypes CBackend where
  btProcDef = error "TODO Back.CBackend BackendTypes instanciation"

instance BackendNames CBackend where
  bnScheduler = error "TODO Back.CBackend BackendNames instanciation"
  bnPiThread = error "TODO Back.CBackend BackendNames instanciation"

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
