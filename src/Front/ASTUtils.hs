{-|
Module         : Front.ASTUtils
Description    : Util functions and typeclass for manipulating the AST
Stability      :

The AST types are defined in 'Front.AST' module, and this module gives functions and typeclasses to
easily manipulate it or extract partial informations from it.
-}
module Front.ASTUtils where

import Front.AST

import Data.List (intercalate)

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

indent :: Int -> String
indent n = intercalate "" $ replicate n " "

instance Show TypeExpr where
  show typ@(TUnknown {}) = "unknown"
  show typ@(TAtom {})    = show $ typAtom typ
  show typ@(TChannel {}) = "chan<" ++ show (typExpr typ) ++ ">"
  show typ@(TTuple {})   = "(" ++ intercalate "," (map show (typExprs typ)) ++ ")"
  show typ@(TPrim {})    = "[" ++ intercalate "," (map show (typArgs typ)) ++ "] -> " ++ show (typRet typ)

instance Show TypeAtom where
  show TBool   = "bool"
  show TInt    = "int"
  show TString = "string"

instance Show Value where
  show val@(VTrue {})   = "true"
  show val@(VFalse {})  = "false"
  show val@(VInt {})    = show $ valInt val
  show val@(VString {}) = valStr val
  show val@(VTuple {})  = "(tuple " ++ unwords (map show $ valVals val) ++ ")"
  show val@(VVar {})    = valVar val
  show val@(VPrim {})   = "(prim #" ++ valModule val ++ "/" ++ valName val ++ args ++ ")"
    where args = if null (valArgs val)
                   then ""
                   else " " ++ unwords (map show $ valArgs val)

instance Show Process where
  show = showProc 0

showProc :: Int -> Process -> String
showProc n (proc@PEnd {}) = indent n ++ "(end)"
showProc n (proc@PChoice {procBranches = []}) = error "empty choice"
showProc n (proc@PChoice {procBranches = [br]}) = showBranch n br
showProc n (proc@PChoice {}) = indent n ++ "(+ \n" ++ brs ++ "\n" ++ indent n ++ ")"
  where brs = intercalate "\n" (map (showBranch (n+3)) (procBranches proc))
showProc n (proc@PCall {}) = indent n ++ "(call #" ++ procModule proc ++ "/" ++ procName proc ++ args ++ ")"
  where args = if null (procArgs proc)
                 then ""
                 else " " ++ unwords (map show $ procArgs proc)

instance Show Branch where
  show = showBranch 0

showBranch :: Int -> Branch -> String
showBranch n branch = indent n ++ "(-> " ++ g ++ " " ++ a ++ "\n" ++ p ++ ")"
  where g = show $ bGuard branch
        a = show $ bAction branch
        p = showProc (n+4) $ bCont branch

instance Show Action where
  show = showAct 0

showAct n (act@ATau {})    = "(tau)"
showAct n (act@AOutput {}) = "(output " ++ actChan act ++ " " ++ show (actData act) ++ ")"
showAct n (act@AInput {})  = "(input " ++ actChan act ++ " " ++ actBind act ++ ")"
showAct n (act@ANew {})    = "(new " ++ actBind act ++ ")"
showAct n (act@ALet {})    = "(let " ++ actBind act ++ " " ++ show (actVal act) ++ ")"
showAct n (act@ASpawn {})  = "(spawn #" ++ actModule act ++ "/" ++ actName act ++ args ++ ")"
  where args = if null (actArgs act)
                 then ""
                 else " " ++ unwords (map show $ actArgs act)
showAct n (act@APrim {})   = "(prim #" ++ actModule act ++ "/" ++ actName act ++ args ++ ")"
  where args = if null (actArgs act)
                 then ""
                 else " " ++ unwords (map show $ actArgs act)

instance Show Definition where
  show = showDef 0

showDef :: Int -> Definition -> String
showDef n def = indent n ++ "(def (" ++ defName def ++ params ++ ")\n" ++ showProc (n+5) (defBody def) ++ "\n" ++ indent n ++ ")"
  where params = if null (defParams def)
                   then ""
                   else " " ++ unwords (map (\(x,_,_) -> x) (defParams def))

instance Show ModuleDef where
  show mDef = "(module " ++ moduleName mDef ++ "\n" ++ ds ++ "\n" ++ ")"
    where ds = intercalate "\n\n" (map (showDef 8) defs)
          defs = moduleDefs mDef

