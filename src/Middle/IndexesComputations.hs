{-|
Module         : Middle.IndexesComputation
Description    : Index of variable computation pass
Stability      : experimental

In runtime, process environment have a fixed size, and each variable is called using an index in this environment.
This module computes these indexes and decorates the AST/
-}
module Middle.IndexesComputations (computingIndexesPass) where

import Front.AST
import PiccError

import Control.Monad.Error
import Control.Monad.Identity


type IndexesM a = ErrorT PiccError Identity a


computingIndexesPass :: ModuleDef -> Either PiccError ModuleDef
computingIndexesPass mDef = runIdentity (runErrorT mDef')
  where mDef' = indModule mDef

indModule :: ModuleDef -> IndexesM ModuleDef
indModule mDef = throwError $ TodoError "IndexesComputation.indModule"

indDefinition :: Definition -> IndexesM Definition
indDefinition def = throwError $ TodoError "IndexesComputation.indDefinition"

indProcess :: Process -> IndexesM Process
indProcess proc@(PEnd {}) = throwError $ TodoError "IndexesComputation.indProcess"
indProcess proc = throwError $ TodoError "IndexesComputation.indProcess"

indBranch :: Branch -> IndexesM Branch
indBranch branch = throwError $ TodoError "IndexesComputation.indBranch"

indAction :: Action -> IndexesM Action
indAction act = throwError $ TodoError "IndexesComputation.indAction"

indValue :: Value -> IndexesM Value
indValue val = throwError $ TodoError "IndexesComputation.indValue"

