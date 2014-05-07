{-|
Module         :
Description    :
Stability      :

Longer description
-}
module Primitives (primTypes) where

import Front.AST
import Front.ASTUtils

import qualified Data.Map as Map

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
