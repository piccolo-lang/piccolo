{-|
Module         : Core.ASTUtils
Description    : Util functions for manipulating the AST
Stability      : experimental

This module defines functions to pretty print AST nodes and to extract
AST informations (such as location).
__TODO__: move the isManagedType to a typecheck file.
avoid Show orphan instances
-}
module Core.ASTUtils where

import Core.AST

isAManagedType :: TypeExpr -> Bool
isAManagedType TChannel {}               = True
isAManagedType TAtom {typAtom = TString} = True
isAManagedType TTuple {}                 = True
isAManagedType _                         = False

isBool :: TypeExpr -> Bool
isBool TAtom {typAtom = TBool} = True
isBool _                       = False

