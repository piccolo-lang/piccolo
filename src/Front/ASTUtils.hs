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

data PrintLevel = SimplePrint
                | PrintTypes
                | PrintIndexes
                deriving (Eq)

-- | The 'AST' typeclass is usefull to define common function on each 'Front.AST' datatype
class AST a where
  localize :: a -> Location  -- ^ extract location information from an AST data
  strSExpr :: [PrintLevel] -> a -> String

instance AST TypeExpr where
  localize = typLoc
  strSExpr _ = show

instance AST Expr where
  localize = exprLoc
  strSExpr = exprSExpr

instance AST Process where
  localize = procLoc
  strSExpr = procSExpr

instance AST Branch where
  localize = brLoc
  strSExpr = brSExpr

instance AST Action where
  localize = actLoc
  strSExpr = actSExpr

instance AST Definition where
  localize = defLoc
  strSExpr = defSExpr

instance AST Modul where
  localize = modLoc
  strSExpr = modSExpr

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

isAManagedType :: TypeExpr -> Bool
isAManagedType TChannel {}               = True
isAManagedType TAtom {typAtom = TString} = True
isAManagedType TTuple {}                 = True
isAManagedType _                         = False

isBool :: TypeExpr -> Bool
isBool TAtom {typAtom = TBool} = True
isBool _                       = False

indent :: Int -> String
indent n = replicate n ' '

modSExpr :: [PrintLevel] -> Modul -> String
modSExpr lvl m = "(module " ++ modName m ++ "\n" ++ sdefs (modDefs m) ++ "\n" ++ ")"
  where sdefs defs = intercalate "\n\n" (map (defSExpr' 8 lvl) defs)

defSExpr :: [PrintLevel] -> Definition -> String
defSExpr = defSExpr' 0

defSExpr' :: Int -> [PrintLevel] -> Definition -> String
defSExpr' ind lvl def = indent ind ++
  "(def (" ++ defName def ++ params ++ ")\n" ++ body ++ "\n" ++
  indent ind ++ ")"
  where params | null (defParams def) = ""
               | otherwise = " " ++ unwords (zipWith (curry param) (defParams def) ([0..] :: [Int]))
        param ((n, t, _), i) = let name = if PrintIndexes `elem` lvl
                                            then "*" ++ show i
                                            else n in
                               let typ  = if PrintTypes `elem` lvl
                                            then "[" ++ show t ++ "]"
                                            else "" in
                               name ++ typ
        body = procSExpr' (ind+5) lvl (defBody def)

procSExpr :: [PrintLevel] -> Process -> String
procSExpr = procSExpr' 0

procSExpr' :: Int -> [PrintLevel] -> Process -> String
procSExpr' ind _ PEnd {}           = indent ind ++ "(end)"
procSExpr' ind lvl proc@PPrefix {} = indent ind ++ act ++ "\n" ++ cont
  where act  = actSExpr lvl (procPref proc)
        cont = procSExpr' ind lvl (procCont proc)
procSExpr' ind lvl proc@PChoice {} = indent ind ++
  "(+ \n" ++ brs ++ "\n" ++ indent ind ++ ")"
  where brs = intercalate "\n" (map (brSExpr' (ind+3) lvl) (procBranches proc))
procSExpr' ind lvl proc@PCall   {} = indent ind ++
  "(call #" ++ procModule proc ++ "/" ++ procName proc ++ args ++ ")"
  where args | null (procArgs proc) = ""
             | otherwise            = " " ++ unwords (map (exprSExpr lvl) $ procArgs proc)

brSExpr :: [PrintLevel] -> Branch -> String
brSExpr = brSExpr' 0

brSExpr' :: Int -> [PrintLevel] -> Branch -> String
brSExpr' ind lvl br@BTau    {} = indent ind ++
  "(-> " ++ guard ++ " tau " ++ "\n" ++ cont ++ ")"
  where guard = exprSExpr lvl $ brGuard br
        cont  = procSExpr' (ind+4) lvl $ brCont br
brSExpr' ind lvl br@BOutput {} = indent ind ++
  "(-> " ++ guard ++ " " ++ act ++ "\n" ++ cont ++ ")"
  where guard = exprSExpr lvl $ brGuard br
        act   = "(output " ++ chan ++ " " ++ exprSExpr lvl (brData br) ++ ")"
        chan  | PrintIndexes `elem` lvl = "*" ++ show (brChanIndex br)
              | otherwise               = brChan br
        cont  = procSExpr' (ind+4) lvl $ brCont br
brSExpr' ind lvl br@BInput  {} = indent ind ++
  "(-> " ++ guard ++ " " ++ act ++ "\n" ++ cont ++ ")"
  where guard = exprSExpr lvl $ brGuard br
        act   = "(input " ++ chan ++ " " ++ bind ++ ")"
        chan  | PrintIndexes `elem` lvl = "*" ++ show (brChanIndex br)
              | otherwise               = brChan br
        bind  | PrintIndexes `elem` lvl = "*" ++ show (brBindIndex br)
              | otherwise               = brBind br
        cont  = procSExpr' (ind+4) lvl $ brCont br

actSExpr :: [PrintLevel] -> Action -> String
actSExpr = actSExpr' 0

actSExpr' :: Int -> [PrintLevel] -> Action -> String
actSExpr' ind lvl act@AOutput {} = indent ind ++
  "(output " ++ channel ++ " " ++ exprSExpr lvl (actData act) ++ ")"
  where channel | PrintIndexes `elem` lvl = "*" ++ show (actChanIndex act)
                | otherwise               = actChan act
actSExpr' ind lvl act@AInput  {} = indent ind ++
  "(input " ++ channel ++ " " ++ bind ++ ")"
  where channel | PrintIndexes `elem` lvl = "*" ++ show (actChanIndex act)
                | otherwise               = actChan act
        bind    | PrintIndexes `elem` lvl = "*" ++ show (actBindIndex act)
                | otherwise               = actBind act
actSExpr' ind lvl act@ANew    {} = indent ind ++
  "(new " ++ actBind act ++ typ ++ ")"
  where typ | PrintTypes `elem` lvl = "[" ++ show (actTyp act) ++ "]"
            | otherwise             = ""
actSExpr' ind lvl act@ALet    {} = indent ind ++
  "(let " ++ actBind act ++ typ ++ " " ++ exprSExpr lvl (actVal act) ++ ")"
  where typ | PrintTypes `elem` lvl = "[" ++ show (actTyp act) ++ "]"
            | otherwise             = ""
actSExpr' ind lvl act@ASpawn  {} = indent ind ++
  "(spawn #" ++ actModule act ++ "/" ++ actName act ++ args ++ ")"
  where args | null (actArgs act) = ""
             | otherwise          = " " ++ unwords (map (exprSExpr lvl) $ actArgs act)
actSExpr' ind lvl act@APrim   {} = indent ind ++
  "(prim #" ++ actModule act ++ "/" ++ actName act ++ args ++ ")"
  where args | null (actArgs act) = ""
             | otherwise          = " " ++ unwords (map (exprSExpr lvl) $ actArgs act)

exprSExpr :: [PrintLevel] -> Expr -> String
exprSExpr = exprSExpr' 0

exprSExpr' :: Int -> [PrintLevel] -> Expr -> String
exprSExpr' ind lvl e@ETrue   {} = indent ind ++ "true" ++
  if PrintTypes `elem` lvl then "[" ++ show (exprTyp e) ++ "]" else ""
exprSExpr' ind lvl e@EFalse  {} = indent ind ++ "false" ++
  if PrintTypes `elem` lvl then "[" ++ show (exprTyp e) ++ "]" else ""
exprSExpr' ind lvl e@EInt    {} = indent ind ++ show (exprInt e) ++
  if PrintTypes `elem` lvl then "[" ++ show (exprTyp e) ++ "]" else ""
exprSExpr' ind lvl e@EString {} = indent ind ++ exprStr e ++
  if PrintTypes `elem` lvl then "[" ++ show (exprTyp e) ++ "]" else ""
exprSExpr' ind lvl e@ETuple  {} = indent ind ++
  "(tuple" ++
  (if PrintTypes `elem` lvl then "[" ++ show (exprTyp e) ++ "]" else "") ++
  " " ++ unwords (map (exprSExpr lvl) $ exprVals e) ++ ")"
exprSExpr' ind lvl e@EVar    {} = indent ind ++
  (if PrintIndexes `elem` lvl then "*" ++ show (exprIndex e) else exprVar e) ++
  if PrintTypes `elem` lvl then "[" ++ show (exprTyp e) ++ "]" else ""
exprSExpr' ind lvl e@EPrim   {} = indent ind ++
  "(prim" ++
  (if PrintTypes `elem` lvl then "[" ++ show (exprTyp e) ++ "]" else "") ++
  " #" ++ exprModule e ++ "/" ++ exprName e ++ args ++ ")"
  where args | null (exprArgs e) = ""
             | otherwise          = " " ++ unwords (map (exprSExpr lvl) $ exprArgs e)
exprSExpr' ind lvl e@EAnd    {} = indent ind ++
  "(and " ++ exprSExpr lvl (exprLeft e) ++ " " ++ exprSExpr lvl (exprRight e) ++ ")"
exprSExpr' ind lvl e@EOr     {} = indent ind ++
  "(or " ++ exprSExpr lvl (exprLeft e) ++ " " ++ exprSExpr lvl (exprRight e) ++ ")"
