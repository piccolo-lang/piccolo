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
data Location = Location { locOffset      :: !Int  -- ^ absolute offset of the block in the file
                         , locStartLine   :: !Int  -- ^ line of the beginning of the block
                         , locStartColumn :: !Int  -- ^ column of the beginning of the block
                         , locEndLine     :: !Int  -- ^ line of the end of the block
                         , locEndColumn   :: !Int  -- ^ column of the end of the block
                         } deriving (Eq, Show)

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
  deriving (Show)

-- | Atomic types are in the separate datatype 'TypeAtom' that is used by 'TypeExpr'
data TypeAtom
  = TBool
  | TInt
  | TString
  deriving (Show, Eq)

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
data Value
  = VTrue   { valTyp :: TypeExpr, valLoc :: Location }
  | VFalse  { valTyp :: TypeExpr, valLoc :: Location }
  | VInt    { valInt  :: Int,    valTyp :: TypeExpr, valLoc :: Location }
  | VString { valStr  :: String, valTyp :: TypeExpr, valLoc :: Location }
  | VTuple  { valVals ::[Value], valTyp :: TypeExpr, valLoc :: Location }
  | VVar    { valVar  :: String, valTyp :: TypeExpr, valLoc :: Location, valIndex :: Int }
  | VPrim   { valModule :: String, valName :: String, valArgs :: [Value], valTyp :: TypeExpr, valLoc :: Location }
  deriving (Show, Eq)

-- | A piccolo program is made of processes. A process expression can be:
--
--   * an inert process 'PEnd'
--
--   * a guarded choice 'PChoice'
--
--   * a call to a process definition 'PCall'
data Process
  = PEnd    { procLoc :: Location }
  | PChoice { procBranches :: [Branch], procLoc :: Location }
  | PCall   { procModule :: String, procName :: String, procArgs :: [Value], procLoc ::  Location }
  deriving (Show, Eq)

-- | A 'Branch' is a possible continuation for a choice process.
-- It is guarded by a boolean value and an action.
data Branch
  = Branch { bGuard  :: Value
           , bAction :: Action
           , bCont   :: Process
           , bLoc    :: Location
           } deriving (Show, Eq)

-- | 'Action' datatype represents the atomic actions of the piccolo language.
-- As contrary as traditionnal presentation of pi-calculus,
-- the "new" constructor is also a prefix action.
-- Moreover, to manipulate value expressions, we defines a "let" and a primitive call action.
-- 'ASpawn' action is used to spawn a parallel process which will be executing the given piccolo process definition.
data Action
  = ATau    { actLoc :: Location }
  | AOutput { actChan :: String, actData :: Value, actLoc :: Location }
  | AInput  { actChan :: String, actBind :: String, actLoc :: Location }
  | ANew    { actBind :: String, actTyp :: TypeExpr, actLoc :: Location }
  | ALet    { actBind :: String, actTyp :: TypeExpr, actVal :: Value, actLoc :: Location }
  | ASpawn  { actModule :: String, actName :: String, actArgs :: [Value], actLoc :: Location }
  | APrim   { actModule :: String, actName :: String, actArgs :: [Value], actLoc :: Location }
  deriving (Show, Eq)

-- | To spawn or (possibly recursively) call a process definition, the 'Definition' type defines
-- a process definition attached with parameter names and types.
data Definition
  = Definition { defName   :: String
               , defParams :: [(String, TypeExpr, Location)]
               , defBody   :: Process
               , defLoc    :: Location
               }

-- | 'ModuleDef' defines the main node of a piccolo AST. It contains several process definitions.
-- If a "Main" definition is defined, it is the entry point when the compiled version of the module
-- will be executed.
data ModuleDef
  = ModuleDef { moduleName :: String
              , moduleDefs :: [Definition]
              , moduleLoc  :: Location
              }

