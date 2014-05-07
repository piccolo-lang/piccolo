{-|
Module         :
Description    :
Stability      :

Longer description
-}
module Front.ASTUtils where

import Front.AST

class AST a where
  localize :: a -> Location

instance AST TypeExpr where
  localize = typLoc

instance AST Value where
  localize = valLoc

instance AST Process where
  localize = procLoc

instance AST Branch where
  localize = bLoc

instance AST Action where
  localize = actLoc

instance AST Definition where
  localize = defLoc

instance AST ModuleDef where
  localize = moduleLoc

noLoc :: Location
noLoc = Location (-1) (-1) (-1) (-1) (-1)

isNoLoc :: Location -> Bool
isNoLoc = (noLoc ==)
