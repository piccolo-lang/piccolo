{-|
Module         : Middle.IndexesComputation
Description    : Index of variable computation pass
Stability      : experimental

In runtime, process environment have a fixed size, and each variable is called using an index in this environment.
This module computes these indexes and decorates the AST/
-}
module Middle.IndexesComputations where

import PiccError
import Front.AST

computingIndexesPass :: ModuleDef -> Either PiccError ModuleDef
computingIndexesPass = error "TODO Middle.IndexesComputations.computingIndexesPass"
