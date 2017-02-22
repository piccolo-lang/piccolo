{-|
Module         : Primitives
Description    : Primitives description
Stability      : experimental

This module gives the type of the primitives that should be supported
by a backend, for the typechecking to be complete.
-}
module Piccolo.Primitives
  ( primTypes
  )
where

import Piccolo.AST

import qualified Data.Map as Map

-- | 'primTypes' is a map from a primitive name to its type.
primTypes :: Map.Map (ModuleName,String) (TypeExpr, [TypeExpr])
primTypes = Map.fromList
  [ ((ModuleName ["core", "arith"], "add"),                (tInt, [tInt, tInt]))
  , ((ModuleName ["core", "arith"], "sub"),                (tInt, [tInt, tInt]))
  , ((ModuleName ["core", "arith"], "mul"),                (tInt, [tInt, tInt]))
  , ((ModuleName ["core", "arith"], "div"),                (tInt, [tInt, tInt]))
  , ((ModuleName ["core", "arith"], "mod"),                (tInt, [tInt, tInt]))
  , ((ModuleName ["core", "arith"], "equals"),             (tBool, [tInt, tInt]))
  , ((ModuleName ["core", "arith"], "not_equals"),         (tBool, [tInt, tInt]))
  , ((ModuleName ["core", "arith"], "less_than"),          (tBool, [tInt, tInt]))
  , ((ModuleName ["core", "arith"], "less_or_eq_than"),    (tBool, [tInt, tInt]))
  , ((ModuleName ["core", "arith"], "greater_than"),       (tBool, [tInt, tInt]))
  , ((ModuleName ["core", "arith"], "greater_or_eq_than"), (tBool, [tInt, tInt]))
  , ((ModuleName ["core", "io"   ], "print_str"),          (tString, [tString]))
  , ((ModuleName ["core", "io"   ], "print_int"),          (tString, [tInt]))
  ]

tBool, tInt, tString :: TypeExpr
tBool   = TAtom TBool   noLoc
tInt    = TAtom TInt    noLoc
tString = TAtom TString noLoc
