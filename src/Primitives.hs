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
  [ (("corearith", "add"),       (tInt, [tInt, tInt]))
  , (("corearith", "substract"), (tInt, [tInt, tInt]))
  , (("corearith", "modulo"),    (tInt, [tInt, tInt]))
  , (("corearith", "equals"),    (tBool, [tInt, tInt]))
  , (("corearith", "less_than"), (tBool, [tInt, tInt]))
  , (("coreio", "print_info"),   (tString, [tString]))
  , (("coreio", "print_str"),    (tString, [tString]))
  , (("coreio", "print_int"),    (tString, [tInt]))
  ]

tBool   :: TypeExpr
tBool   = TAtom TBool   noLoc
tInt    :: TypeExpr
tInt    = TAtom TInt    noLoc
tString :: TypeExpr
tString = TAtom TString noLoc
