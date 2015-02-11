{-|
Module         : Front.ASTUtils
Description    : Util functions for manipulating the AST
Stability      : experimental

This module defines functions to pretty print AST nodes and to extract
AST informations (such as location).
__TODO__: move the isManagedType to a typecheck file.
__TODO__: move the content of this file into Front.AST.hs to
avoid Show orphan instances
-}
module Front.ASTUtils where

import Front.AST

import Data.List (intercalate)

-- * Locations

-- | The 'AST' typeclass defines pretty printer and localize function
class AST a where
  localize :: a -> Location

instance AST TypeExpr where
  localize = typLoc

instance AST Expr where
  localize = exprLoc

instance AST Process where
  localize = procLoc

instance AST Branch where
  localize = brLoc

instance AST Action where
  localize = actLoc

instance AST Definition where
  localize = defLoc

instance AST Modul where
  localize = modLoc

-- | 'noLoc' is a fake location used when there is no code correspondance
-- for an AST node (for example, an inferred type is not in the initial code).
noLoc :: Location
noLoc = Location (-1) (-1) (-1) (-1) (-1)

-- | The 'isNoLoc' predicate tests if a given location is fake.
isNoLoc :: Location -> Bool
isNoLoc = (noLoc ==)

isAManagedType :: TypeExpr -> Bool
isAManagedType TChannel {}               = True
isAManagedType TAtom {typAtom = TString} = True
isAManagedType TTuple {}                 = True
isAManagedType _                         = False

isBool :: TypeExpr -> Bool
isBool TAtom {typAtom = TBool} = True
isBool _                       = False

-- * Pretty printing

indent :: Int -> String
indent n = take (2 * n) (repeat ' ')

instance Show Process where
  show = ppProcess 0

ppProcess :: Int -> Process -> String
ppProcess n (PEnd {}) =
  indent n ++ "end"
ppProcess n proc@PPrefix {} =
  ppAction n (procPref proc) ++ ",\n" ++
  ppProcess n (procCont proc)
ppProcess n proc@PChoice {} =
  indent n ++ intercalate ("\n" ++ indent (n - 1) ++ "+ ")
                          (map (ppBranch 0) (procBranches proc))
ppProcess n proc@PCall   {} =
  indent n ++ procModule proc ++ "/" ++ procName proc ++ "(" ++
  intercalate ", " (map show (procArgs proc)) ++ ")"

instance Show Branch where
  show = ppBranch 0

ppBranch :: Int -> Branch -> String
ppBranch n br@BTau    {} =
  indent n ++ "[" ++ show (brGuard br) ++ "] " ++
  "tau,\n" ++ ppProcess (n + 1) (brCont br)
ppBranch n br@BOutput {} =
  indent n ++ "[" ++ show (brGuard br) ++ "] " ++
  brChan br ++ "!" ++ show (brData br) ++ ",\n" ++
  ppProcess (n + 1) (brCont br)
ppBranch n br@BInput  {} =
  indent n ++ "[" ++ show (brGuard br) ++ "] " ++
  brChan br ++ "?(" ++ brBind br ++ "),\n" ++
  ppProcess (n + 1) (brCont br)

instance Show Action where
  show = ppAction 0

ppAction :: Int -> Action -> String
ppAction n act@AOutput {} =
  indent n ++ actChan act ++ "!" ++ show (actData act)
ppAction n act@AInput  {} =
  indent n ++ actChan act ++ "?(" ++ actBind act ++ ")"
ppAction n act@ANew    {} =
  indent n ++ "new (" ++ actBind act ++ ": " ++ show (actTyp act) ++ ")"
ppAction n act@ALet    {} =
  indent n ++ "let (" ++ actBind act ++ ": " ++ show (actTyp act) ++
  " = " ++ show (actVal act) ++ ")"
ppAction n act@ASpawn  {} =
  indent n ++ "spawn {" ++
  actModule act ++ "/" ++ actName act ++ "(" ++ args ++ ")}"
  where args = intercalate ", " $ map show (actArgs act)
ppAction n act@APrim   {} =
  indent n ++ actModule act ++ "/" ++ actName act ++ "(" ++ args ++ ")"
  where args = intercalate ", " $ map show (actArgs act)

instance Show Definition where
  show = ppDefinition 0

ppDefinition :: Int -> Definition -> String
ppDefinition n def =
  indent n ++
  "def " ++ defName def ++ "(" ++ params ++ ") =\n" ++
  ppProcess (n + 1) (defBody def) ++ "\n"
  where params = intercalate ", " $ map showParam (defParams def)
        showParam (name, typ, _) = name ++ ": " ++ show typ

instance Show Modul where
  show = ppModul 0

ppModul :: Int -> Modul -> String
ppModul n modul =
  indent n ++
  "module " ++ modName modul ++ "\n\n" ++
  intercalate "\n" (map (ppDefinition n) (modDefs modul)) ++
  "\n"

