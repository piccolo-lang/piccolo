{-|
Module         : Front.AST
Description    : Piccolo AST datatypes
Stability      : experimental

This module defines the datatypes for piccolo ASTs description. Each type of the module
is marked with a location data, to print better error messages and track code locations
during the whole compilation process.
-}
module Front.AST where

-- | The 'Location' type is a record of line and columns informations
data Location
  = Location { locOffset      :: !Int  -- ^ absolute offset of the block in the file
  , locStartLine   :: !Int  -- ^ line of the beginning of the block
  , locStartColumn :: !Int  -- ^ column of the beginning of the block
  , locEndLine     :: !Int  -- ^ line of the end of the block
  , locEndColumn   :: !Int  -- ^ column of the end of the block
  } deriving (Eq)

instance Show Location where
  show loc = show (locStartLine loc) ++ ":" ++ show (locStartColumn loc) ++ "->" ++
             show (locEndLine loc)   ++ ":" ++ show (locEndColumn loc)

--}

-- | In the core piccolo language type expressions are simple. There are:
--
--   * atomic types suc has Bool, Int or String,
--
--   * channel types of the form \<T\> where T is a type expression,
--
--   * tuple types of the form T1\*...*TN.
data TypeExpr
  = TUnknown { typLoc :: Location }
  | TAtom    { typAtom :: TypeAtom, typLoc :: Location }
  | TChannel { typExpr :: TypeExpr, typLoc :: Location }
  | TTuple   { typExprs :: [TypeExpr], typLoc :: Location }
  | TPrim    { typArgs :: [TypeExpr], typRet :: TypeExpr, typLoc :: Location }

-- | Atomic types are in the separate datatype 'TypeAtom' which is used by 'TypeExpr'
data TypeAtom
  = TBool
  | TInt
  | TString
  deriving (Eq)

-- | 'TypeExpr' expressions are compared upto location data
instance Eq TypeExpr where
  (==) (TUnknown {})              (TUnknown {})              = True
  (==) (TAtom { typAtom = a })    (TAtom { typAtom = b })    = a == b
  (==) (TChannel { typExpr = a }) (TChannel { typExpr = b }) = a == b
  (==) (TTuple { typExprs = a })  (TTuple { typExprs = b })  = a == b
  (==) (TPrim { typArgs = a, typRet = r }) (TPrim { typArgs = b, typRet = s }) =
    a == b && r == s
  (==) _ _ = False

-- | 'Value' expressions are manipulated by piccolo processes
-- They can be boolean, integers or string values,
-- variable or primitive call.
-- Each value is tagged with its type and location in the piccolo program.
-- Moreover, variables are tagged with an index in current environment for nameless compilation.
data Expr
  = ETrue   { exprTyp :: TypeExpr, exprLoc :: Location }
  | EFalse  { exprTyp :: TypeExpr, exprLoc :: Location }
  | EInt    { exprTyp :: TypeExpr, exprLoc :: Location, exprInt  :: Int }
  | EString { exprTyp :: TypeExpr, exprLoc :: Location, exprStr  :: String }
  | ETuple  { exprTyp :: TypeExpr, exprLoc :: Location, exprVals :: [Expr] }
  | EVar    { exprTyp :: TypeExpr, exprLoc :: Location, exprVar :: String, exprIndex :: Int }
  | EPrim   { exprTyp :: TypeExpr, exprLoc :: Location, exprModule :: String, exprName :: String, exprArgs :: [Expr] }
  | EAnd    { exprTyp :: TypeExpr, exprLoc :: Location, exprLeft :: Expr, exprRight :: Expr }
  | EOr     { exprTyp :: TypeExpr, exprLoc :: Location, exprLeft :: Expr, exprRight :: Expr }

data Process
  = PEnd    { procLoc :: Location }
  | PPrefix { procPref :: Action, procCont :: Process, procLoc :: Location }
  | PChoice { procBranches :: [Branch], procLoc :: Location }
  | PCall   { procModule :: String, procName :: String, procArgs :: [Expr], procLoc ::  Location }

data Branch
  = BTau    { brGuard :: Expr, brCont :: Process, brLoc :: Location }
  | BOutput { brGuard :: Expr, brChan :: String, brData :: Expr, brChanIndex :: Int, brCont :: Process, brLoc :: Location }
  | BInput  { brGuard :: Expr, brChan :: String, brBind :: String, brBindTyp :: TypeExpr, brChanIndex :: Int, brBindIndex :: Int, brCont :: Process, brLoc :: Location }

-- | 'Action' datatype represents the atomic actions of the piccolo language.
-- As contrary as traditionnal presentation of pi-calculus,
-- the "new" constructor is also a prefix action.
-- Moreover, to manipulate value expressions, we defines a "let" and a primitive call action.
-- 'ASpawn' action is used to spawn a parallel process which will be executing the given piccolo process definition.
data Action
  = AOutput { actChan :: String, actData :: Expr, actChanIndex :: Int, actLoc :: Location }
  | AInput  { actChan :: String, actBind :: String, actBindTyp :: TypeExpr, actChanIndex :: Int, actBindIndex :: Int, actLoc :: Location }
  | ANew    { actBind :: String, actBindIndex :: Int, actTyp :: TypeExpr, actLoc :: Location }
  | ALet    { actBind :: String, actBindIndex :: Int, actTyp :: TypeExpr, actVal :: Expr, actLoc :: Location }
  | ASpawn  { actModule :: String, actName :: String, actArgs :: [Expr], actLoc :: Location }
  | APrim   { actModule :: String, actName :: String, actArgs :: [Expr], actLoc :: Location }

-- | To spawn or (possibly recursively) call a process definition, the 'Definition' type defines
-- a process definition attached with parameter names and types.
data Definition
  = Definition { defName    :: String
               , defParams  :: [(String, TypeExpr, Location)]
               , defBody    :: Process
               , defEnvSize :: Int
               , defLoc     :: Location
               }

-- | 'ModuleDef' defines the main node of a piccolo AST. It contains several process definitions.
-- If a "Main" definition is defined, it is the entry point when the compiled version of the module
-- will be executed.
data Modul
  = Modul { modName :: String
          , modDefs :: [Definition]
          , modLoc  :: Location
          }

