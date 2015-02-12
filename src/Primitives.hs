{-|
Module         : Primitives
Description    : Primitives description
Stability      : experimental

This module gives the type of the primitives that should be supported
by a backend, for the typechecking to be complete.
-}
module Primitives
  ( primTypes
  )
where

import Core.AST

import qualified Data.Map as Map

-- | 'primTypes' is a map from a primitive name to its type.
primTypes :: Map.Map (String,String) (TypeExpr, [TypeExpr])
primTypes = Map.fromList
  [ (("core/arith", "add"),       (tInt, [tInt, tInt]))
  , (("core/arith", "sub"),       (tInt, [tInt, tInt]))
  , (("core/arith", "mul"),       (tInt, [tInt, tInt]))
  , (("core/arith", "div"),       (tInt, [tInt, tInt]))
  , (("core/arith", "mod"),       (tInt, [tInt, tInt]))
  , (("core/arith", "equals"),    (tBool, [tInt, tInt]))
  , (("core/arith", "less_than"), (tBool, [tInt, tInt]))
  , (("core/io", "print_info"),   (tString, [tString]))
  , (("core/io", "print_str"),    (tString, [tString]))
  , (("core/io", "print_int"),    (tString, [tInt]))
  ]

tBool, tInt, tString :: TypeExpr
tBool   = TAtom TBool   noLoc
tInt    = TAtom TInt    noLoc
tString = TAtom TString noLoc
