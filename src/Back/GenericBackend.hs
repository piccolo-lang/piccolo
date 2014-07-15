{-|
Module         : Back.GenericBackend
Description    : A generic backend module
Stability      : experimental

This backend targets pseudo-code, as the one in which compilation schemes of the documentation
are written.
-}
module Back.GenericBackend where

import Back.SeqAST

emitCode = error "emitCode not yet implemented"


--instance Backend GenericBackend where
--  emitVarName = error "TODO GenericBackend.emitVarName"
--  emitPiccType = error "TODO GenericBackend.emitPiccType"
--  emitExpr = error "TODO GenericBackend.emitExpr"
--  emitBinop = error "TODO GenericBackend.emitBinop"
--  emitUnop = error "TODO GenericBackend.emitUnop"
--  emitInstr = error "TODO GenericBackend.emitInstr"
--  emitCode = error "TODO GenericBackend.emitCode"

