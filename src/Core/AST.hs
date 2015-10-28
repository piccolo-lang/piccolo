{-|
Module         : Core.AST
Description    : Piccolo AST datatypes
Stability      : experimental

This module defines the datatypes for piccolo-core AST description.
Each type of the module is marked with a location data,
to print better error messages and track code locations
during the whole compilation process.

__TODO__: do not mangle the ModuleName/SubModuleName in the AST.
-}
module Core.AST
  ( Location (..), noLoc, isNoLoc
  , AST (..)
  , TypeExpr (..)
  , TypeAtom (..)
  , Expr (..)
  , Process (..)
  , Branch (..)
  , Action (..)
  , Definition (..)
  , Modul (..)
  , 
  )
where

import Data.List (intercalate)


-- * Locations

-- | The 'Location' type is a record of line and column informations
data Location = Location
  { locOffset      :: !Int  -- ^ absolute offset of the block in the file
  , locStartLine   :: !Int  -- ^ line of the beginning of the block
  , locStartColumn :: !Int  -- ^ column of the beginning of the block
  , locEndLine     :: !Int  -- ^ line of the end of the block
  , locEndColumn   :: !Int  -- ^ column of the end of the block
  } deriving (Eq)

instance Show Location where
  show loc = show (locStartLine loc) ++ ":" ++
             show (locStartColumn loc) ++ "->" ++
             show (locEndLine loc)   ++ ":" ++
             show (locEndColumn loc)

-- | The 'AST' typeclass defines a localization function
class Show a => AST a where
  localize :: a -> Location

-- | 'noLoc' is a fake location used when there is no code correspondance
-- for an AST node (for example, an inferred type is not in the initial code).
noLoc :: Location
noLoc = Location (-1) (-1) (-1) (-1) (-1)

-- | The 'isNoLoc' predicate tests if a given location is fake.
isNoLoc :: Location -> Bool
isNoLoc = (noLoc ==)


-- * AST types

-- | Piccolo-core type expressions
data TypeExpr
  = TUnknown
    { typLoc :: Location
    } -- ^ unknown type, used for future inference
  | TAtom
    { typAtom :: TypeAtom
    , typLoc  :: Location
    }
  | TChannel
    { typExpr :: TypeExpr
    , typLoc  :: Location
    }
  | TTuple
    { typExprs :: [TypeExpr]
    , typLoc   :: Location
    }
  | TPrim
    { typArgs :: [TypeExpr]
    , typRet  :: TypeExpr
    , typLoc  :: Location
    }

instance Show TypeExpr where
  show TUnknown     {} = "unknown"
  show typ@TAtom    {} = show $ typAtom typ
  show typ@TChannel {} = "chan<" ++ show (typExpr typ) ++ ">"
  show typ@TTuple   {} = "(" ++ intercalate "," (map show (typExprs typ)) ++ ")"
  show typ@TPrim    {} = "[" ++ intercalate "," (map show (typArgs typ)) ++
                         "] -> " ++ show (typRet typ)

instance AST TypeExpr where
  localize = typLoc

-- | Atomic types
data TypeAtom
  = TBool
  | TInt
  | TString
  deriving (Eq)

instance Show TypeAtom where
  show TBool   = "bool"
  show TInt    = "int"
  show TString = "string"


-- | 'TypeExpr' expressions are compared upto location data
instance Eq TypeExpr where
  (==) (TUnknown {})              (TUnknown {})              = True
  (==) (TAtom { typAtom = a })    (TAtom { typAtom = b })    = a == b
  (==) (TChannel { typExpr = a }) (TChannel { typExpr = b }) = a == b
  (==) (TTuple { typExprs = a })  (TTuple { typExprs = b })  = a == b
  (==) (TPrim { typArgs = a, typRet = r }) (TPrim { typArgs = b, typRet = s }) =
    a == b && r == s
  (==) _ _ = False


-- * Languages expressions

-- | 'Value' expressions are manipulated by piccolo threads.
-- Each value is tagged with its type.
-- Moreover, variables are tagged with an index in current environment
-- for nameless generated code.
data Expr
  = ETrue
    { exprTyp :: TypeExpr
    , exprLoc :: Location
    }
  | EFalse
    { exprTyp :: TypeExpr
    , exprLoc :: Location
    }
  | EInt
    { exprTyp :: TypeExpr
    , exprLoc :: Location
    , exprInt :: Int
    }
  | EString
    { exprTyp :: TypeExpr
    , exprLoc :: Location
    , exprStr :: String
    }
  | ETuple
    { exprTyp  :: TypeExpr
    , exprLoc  :: Location
    , exprVals :: [Expr]
    }
  | EVar
    { exprTyp   :: TypeExpr
    , exprLoc   :: Location
    , exprVar   :: String
    , exprIndex :: Int
    }
  | EPrim
    { exprTyp    :: TypeExpr
    , exprLoc    :: Location
    , exprModule :: String
    , exprName   :: String
    , exprArgs   :: [Expr]
    }
  | EAnd
    { exprTyp   :: TypeExpr
    , exprLoc   :: Location
    , exprLeft  :: Expr
    , exprRight :: Expr
    }
  | EOr
    { exprTyp   :: TypeExpr
    , exprLoc   :: Location
    , exprLeft  :: Expr
    , exprRight :: Expr
    }

instance Show Expr where
  show ETrue     {} = "true"
  show EFalse    {} = "false"
  show e@EInt    {} = show $ exprInt e
  show e@EString {} = exprStr e
  show e@ETuple  {} = "(" ++ intercalate ", " (map show (exprVals e)) ++ ")"
  show e@EVar    {} = exprVar e
  show e@EPrim   {} = "#" ++ exprModule e ++ "/" ++ exprName e ++
                      "(" ++ args ++ ")"
    where args = intercalate ", " (map show (exprArgs e))
  show e@EAnd    {} = "(" ++ show (exprLeft e) ++ ") and (" ++
                      show (exprRight e) ++ ")"
  show e@EOr     {} = "(" ++ show (exprLeft e) ++ ") or (" ++
                      show (exprRight e) ++ ")"

instance AST Expr where
  localize = exprLoc


-- * Concurrency AST constructions

-- | Process node
data Process
  = PEnd
    { procLoc :: Location
    } -- ^ inert process
  | PPrefix
    { procPref :: Action
    , procCont :: Process
    , procLoc  :: Location
    } -- ^ prefix action
  | PChoice
    { procBranches :: [Branch]
    , procSafe     :: Bool
    , procLoc      :: Location
    } -- ^ guarded choice
  | PCall
    { procModule :: String
    , procName   :: String
    , procArgs   :: [Expr]
    , procLoc    ::  Location
    } -- ^ piccolo process definition call

instance Show Process where
  show = ppProcess 0

instance AST Process where
  localize = procLoc

-- | Choice branches
data Branch
  = BTau
    { brGuard :: Expr
    , brCont  :: Process
    , brLoc   :: Location
    }
  | BOutput
    { brGuard     :: Expr
    , brChan      :: String
    , brData      :: Expr
    , brChanIndex :: Int
    , brCont      :: Process
    , brLoc       :: Location
    }
  | BInput
    { brGuard     :: Expr
    , brChan      :: String
    , brBind      :: String
    , brBindTyp   :: TypeExpr
    , brChanIndex :: Int
    , brBindIndex :: Int
    , brCont      :: Process
    , brLoc       :: Location
    }

instance Show Branch where
  show = ppBranch 0

instance AST Branch where
  localize = brLoc

-- | 'Action' datatype represents the atomic actions of the
-- piccolo-core language
data Action
  = AOutput
    { actChan      :: String
    , actData      :: Expr
    , actChanIndex :: Int
    , actLoc       :: Location
    }
  | AInput
    { actChan      :: String
    , actBind      :: String
    , actBindTyp   :: TypeExpr
    , actChanIndex :: Int
    , actBindIndex :: Int
    , actLoc       :: Location
    }
  | ANew
    { actBind      :: String
    , actBindIndex :: Int
    , actTyp       :: TypeExpr
    , actLoc       :: Location
    }
  | ALet
    { actBind      :: String
    , actBindIndex :: Int
    , actTyp       :: TypeExpr
    , actVal       :: Expr
    , actLoc       :: Location
    }
  | ASpawn
    { actModule :: String
    , actName   :: String
    , actArgs   :: [Expr]
    , actLoc    :: Location
    }
  | APrim
    { actModule :: String
    , actName   :: String
    , actArgs   :: [Expr]
    , actLoc    :: Location
    }

instance Show Action where
  show = ppAction 0

instance AST Action where
  localize = actLoc


-- * Definitions and Modules

-- | To spawn or (possibly recursively) call a process definition,
-- the 'Definition' type defines
-- a process definition attached with parameter names and types.
data Definition
  = Definition
    { defName    :: String
    , defParams  :: [(String, TypeExpr, Location)]
    , defBody    :: Process
    , defEnvSize :: Int
    , defLoc     :: Location
    }

instance Show Definition where
  show = ppDefinition 0

instance AST Definition where
  localize = defLoc

-- | 'ModuleDef' defines the main node of a piccolo AST.
-- It contains several process definitions.
-- If a "Main" definition is defined,
-- it is the entry point when the compiled version of the module
-- will be executed.
data Modul
  = Modul
    { modName :: String
    , modDefs :: [Definition]
    , modLoc  :: Location
    }

instance Show Modul where
  show = ppModul 0

instance AST Modul where
  localize = modLoc


-- * Pretty printing

indent :: Int -> String
indent n = replicate (2 * n) ' '

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

ppDefinition :: Int -> Definition -> String
ppDefinition n def =
  indent n ++
  "def " ++ defName def ++ "(" ++ params ++ ") =\n" ++
  ppProcess (n + 1) (defBody def) ++ "\n"
  where params = intercalate ", " $ map showParam (defParams def)
        showParam (name, typ, _) = name ++ ": " ++ show typ

ppModul :: Int -> Modul -> String
ppModul n modul =
  indent n ++
  "module " ++ modName modul ++ "\n\n" ++
  intercalate "\n" (map (ppDefinition n) (modDefs modul)) ++
  "\n"

