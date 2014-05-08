{-|
Module         : Front.ASTUtils
Description    : Util functions and typeclass for manipulating the AST
Stability      :

The AST types are defined in 'Front.AST' module, and this module gives functions and typeclasses to
easily manipulate it or extract partial informations from it.
-}
module Front.ASTUtils where

import Front.AST

-- | The 'AST' typeclass is usefull to define common function on each 'Front.AST' datatype
class AST a where
  localize :: a -> Location  -- ^ extract location information from an AST data

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

-- | 'noLoc' is a value used as a fake location.
-- It's used for example to give a location when annotating AST with types,
-- since the inferred type for a value was not necessarily present in the programmer's version
-- of a piccolo program.
noLoc :: Location
noLoc = Location (-1) (-1) (-1) (-1) (-1)

-- | The 'isNoLoc' predicates test if a given location is only a fake location for an AST element
-- that was not on the programmer's version of a piccolo program.
isNoLoc :: Location -> Bool
isNoLoc = (noLoc ==)
