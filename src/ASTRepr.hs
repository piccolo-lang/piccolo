
{--

# AST Representation #

The module `ASTRepr` fournit la représentation
des arbres de syntaxe abstraite en sortie de Parsing.

--}

module ASTRepr where

{--

Each node of the AST is associated to a `Location`
 corresponding to the parsed source.

--}

data Location = Location {
  locOffset :: Int,
  locStartColumn :: Int,
  locEndColumn :: Int,
  locStartLine :: Int,
  locEndLine ::Int
  } deriving (Eq, Show)

{--

## Type expressions ##

In the core piccolo language the type expressions are
very simple. There are :

 - *atomic types* such as `Bool`, `Int` or `String`,
 - *channel types* of the form `<T>` where `T` is a type expression,
 - *tuple types* of the form `T1*T2*...*TN`

The type `TUnknown` is used as a placeholder for (local, naive) inference.

--}

data TypeExpr =
  TUnknown
  | TAtom TypeAtom Location
  | TChannel TypeExpr Location
  | TTuple [TypeExpr] Location
  deriving (Show, Eq)
           
data TypeAtom =
  TBool
  | TInt
  | TString
  deriving (Show, Eq)

{--

## Value expressions ##

--}

data Value =
  VTrue Location
  | VFalse Location
  | VInt Int Location
  | VString String Location
  | VVar String Location -- <variable-name>
  | VPrim String String [Value] Location  -- <lib-id> <primitive-id> [<arguments>]
  deriving (Show, Eq)

{--

## Process expressions ##

--}

data Process =
  PEnd Location
  | PLet [(String,Value)] Process Location -- [(<var>, <val>)] <proc>
  | PChoice [Branch] Location
  | PCall String String [Value] Location   -- tail call <module-id> <def-id> [<arguments>]
  deriving (Show, Eq)

data Branch = Branch {
  bGuard :: Value,
  bAction :: Action,
  bCont :: Process,
  bLoc :: Location
  } deriving (Show, Eq)

data Action =
  Tau Location
  | Output String Value Location  -- <channel> <data> 
  | Input String String Location -- <channel> <var>
  | New String TypeExpr Location -- new <channel> <type>
  | Spawn String String [Value] Location -- spawn call <module-id> <def-id> [<arguments>]
  | Prim String String [Value] Location -- primitite call <lib-id> <def-id> [<arguments>]
    deriving (Show, Eq)

{--

## Process definitions ##

--}

data Definition = Definition {
  defName :: String,
  defParams :: [(String, TypeExpr, Location)],
  defBody :: Process
  }

{--

## Modules definitions ##

--}

data ModuleDef = ModuleDef {
  moduleName :: String,
  moduleDefs :: [Definition],
  moduleLoc :: Location
  }

