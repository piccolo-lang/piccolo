
{--

# AST Representation #

The module `ASTRepr` fournit la représentation
des arbres de syntaxe abstraite en sortie de Parsing.

--}

module Front.AST where

import Utils.Location

{--

## Type expressions ##

In the core piccolo language the type expressions are
very simple. There are :

 - *atomic types* such as `Bool`, `Int` or `String`,
 - *channel types* of the form `<T>` where `T` is a type expression,
 - *tuple types* of the form `T1*T2*...*TN`

The type `TUnknown` is used as a placeholder for (local, naive) inference.

--}

data TypeExpr
  = TUnknown
  | TAtom    { typAtom :: TypeAtom, typLoc :: Location }
  | TChannel { typExpr :: TypeExpr, typLoc :: Location }
  | TTuple   { typExprs :: [TypeExpr], typLoc :: Location }
  deriving (Show, Eq)
           
data TypeAtom
  = TBool
  | TInt
  | TString
  deriving (Show, Eq)

{--

## Value expressions ##

--}

data Value
  = VTrue   { valLoc :: Location }
  | VFalse  { valLoc :: Location }
  | VInt    { valInt :: Int, valLoc :: Location }
  | VString { valStr :: String, valLoc :: Location }
  | VTuple  { valVals ::[Value], valLoc :: Location }
  | VVar    { valVar :: String, valLoc :: Location }
  | VPrim   { valModule :: String, valName :: String, valArgs :: [Value], valLoc :: Location }
  deriving (Show, Eq)

{--

## Process expressions ##

--}

data Process
  = PEnd    { procLoc :: Location }
  | PChoice { procBranches :: [Branch], procLoc :: Location }
  | PCall   { procModule :: String, procName :: String, procArgs :: [Value], procLoc ::  Location }
  deriving (Show, Eq)

data Branch
  = Branch { bGuard  :: Value
           , bAction :: Action
           , bCont   :: Process
           , bLoc    :: Location
           } deriving (Show, Eq)

data Action
  = ATau    { actLoc :: Location }
  | AOutput { actChan :: String, actData :: Value, actLoc :: Location }
  | AInput  { actChan :: String, actBind :: String, actLoc :: Location }
  | ANew    { actBind :: String, actTyp :: TypeExpr, actLoc :: Location }
  | ALet    { actBind :: String, actTyp :: TypeExpr, actVal :: Value, actLoc :: Location }
  | ASpawn  { actModule :: String, actName :: String, actArgs :: [Value], actLoc :: Location }
  | APrim   { actModule :: String, actName :: String, actArgs :: [Value], actLoc :: Location }
  deriving (Show, Eq)

{--

## Process definitions ##

--}

data Definition
  = Definition { defName   :: String
               , defParams :: [(String, TypeExpr, Location)]
               , defBody   :: Process
               , defLoc    :: Location
               }

{--

## Modules definitions ##

--}

data ModuleDef
  = ModuleDef { moduleName :: String
              , moduleDefs :: [Definition]
              , moduleLoc  :: Location
              }

