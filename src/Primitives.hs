{-|
Module         : Front.Primitives
Description    : Primitives description
Stability      : experimental

This module gives the type of the primitives that should be supported by a backend,
for the typechecking to be complete.
-}
module Primitives (primTypes) where

import Front.AST
import Front.ASTUtils

import qualified Data.Map as Map

-- | 'primTypes' is a map from a primitive name to the type of the primitive.
-- Argument is made of the name of the module containing the primitive and the primitive name.
-- The function return the return 'TypeExpr' of the primitive, and a list of the
-- 'TypeExpr' of its arguments.
primTypes :: Map.Map (String,String) (TypeExpr, [TypeExpr])
primTypes = Map.fromList
  [ (("core/arith", "add"),       (tInt, [tInt, tInt]))
  , (("core/arith", "substract"), (tInt, [tInt, tInt]))
  , (("core/arith", "modulo"),    (tInt, [tInt, tInt]))
  , (("core/arith", "equals"),    (tBool, [tInt, tInt]))
  , (("core/arith", "less_than"), (tBool, [tInt, tInt]))
  , (("core/io", "print_info"),   (tString, [tString]))
  , (("core/io", "print_str"),    (tString, [tString]))
  , (("core/io", "print_int"),    (tString, [tInt]))
  ]

tBool   :: TypeExpr
tBool   = TAtom TBool   noLoc
tInt    :: TypeExpr
tInt    = TAtom TInt    noLoc
tString :: TypeExpr
tString = TAtom TString noLoc
