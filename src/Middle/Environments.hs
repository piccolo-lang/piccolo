{-|
Module         : Middle.Environments
Description    : Indexes of variables computation pass
Stability      : experimental

In runtime, process environment have a fixed size, and each variable is called using an index in this environment.
This module computes these indexes and decorates the AST/
-}
module Middle.Environments (computingEnvPass, EnvSize) where

import Front.AST
import PiccError

import Data.List
import qualified Data.Map as Map
import Control.Monad.Error
import Control.Monad.State
import Debug.Trace


type EnvSize = Int
type DefsEnv = Map.Map String EnvSize

-- (current size of all defs in the module,
--  called defs,
--  variables)
type Environment = (DefsEnv, DefsEnv, [String])
type EnvM a = ErrorT PiccError (State Environment) a

lookupSizeOfDef :: String -> EnvM EnvSize
lookupSizeOfDef name = do
  (defs, _, _) <- get
  return $ defs Map.! name

lookupVar :: String -> EnvM (Maybe Int)
lookupVar v = do
  (_, _, env) <- get
  return $ elemIndex v env

allocVar :: String -> EnvM Int
allocVar v = do
  ind <- lookupVar v
  case ind of
    Just i  -> return i
    Nothing -> do
      (defs, called, env) <- get
      put (defs, called, env ++ [v])
      return $ length env

resetEnvs :: [String] -> EnvM ()
resetEnvs args = do
  (defs, _, _) <- get
  put (defs, Map.empty, args)

computeDefEnvSize :: EnvM EnvSize
computeDefEnvSize = do
  (_, called, env) <- get
  let mCalled = if Map.null called then 0 else maximum $ Map.elems called
  let mEnv    = if null env    then 0 else length env
  return $ maximum [mCalled, mEnv]

registerSize :: String -> EnvSize -> EnvM ()
registerSize name size = do
  (defs, e, f) <- get
  put (Map.insert name size defs, e, f)


-- | The 'computingEnvPass' function computes environment that are needed to
-- address variable with a simple integer at the runtime. It also decorates
-- the AST with those indexes.
computingEnvPass :: Modul -> Either PiccError Modul
computingEnvPass m = loopUntilFix m $ Map.fromList (map extractSize (modDefs m))
  where extractSize def = let i = defEnvSize def in
                          let n = defName def in
                          if i < 0 then (n, 0) else (n, i)
        loopUntilFix modul defSizes = let (result, (defSizes', _, _)) = runState (runErrorT (envModule modul)) (defSizes, Map.empty, []) in
                                    case result of
                                      Left err   -> Left err
                                      Right modul' -> if defSizes == defSizes'
                                                      then Right modul'
                                                      else loopUntilFix modul' defSizes'

envModule :: Modul -> EnvM Modul
envModule m = do
  defs <- mapM envDefinition $ modDefs m
  return $ m { modDefs = defs }

envDefinition :: Definition -> EnvM Definition
envDefinition def = do
  resetEnvs $ map (\(x,_,_) -> x) (defParams def)
  proc <- envProcess $ defBody def
  size <- computeDefEnvSize
  registerSize (defName def) size
  return $ def { defBody = proc }

envProcess :: Process -> EnvM Process
envProcess proc@PEnd    {} = return proc
envProcess proc@PPrefix {} = do
  pref <- envAction $ procPref proc
  cont <- envProcess $ procCont proc
  return proc { procPref = pref, procCont = cont }
envProcess proc@PChoice {} = do
  branches <- mapM envBranch $ procBranches proc
  return proc { procBranches = branches }
envProcess proc@PCall { procName = name } = do
  (e, called, f) <- get
  callSize <- lookupSizeOfDef name
  put (e, Map.insert name callSize called, f)
  args <- mapM envExpr $ procArgs proc
  return proc { procArgs = args }

envBranch :: Branch -> EnvM Branch
envBranch br@BTau    {} = do
  guard <- envExpr $ brGuard br
  cont  <- envProcess $ brCont br
  return br { brGuard = guard, brCont = cont }
envBranch br@BOutput {} = do
  guard <- envExpr $ brGuard br
  chanInd <- lookupVar $ brChan br
  case chanInd of
    Just i  -> do
      cont <- envProcess $ brCont br
      return br { brGuard = guard, brChanIndex = i, brCont = cont }
    Nothing -> throwError $ SimpleError "var not in scope"
envBranch br@BInput  {} = do
  guard <- envExpr $ brGuard br
  chanInd <- lookupVar $ brChan br
  case chanInd of
    Nothing -> throwError $ SimpleError "var not in scope"
    Just i  -> do
      j <- allocVar $ brBind br
      cont <- envProcess $ brCont br
      return br { brGuard = guard, brChanIndex = i, brBindIndex = j, brCont = cont }

envAction :: Action -> EnvM Action
envAction act@AOutput {} = do
  ind <- lookupVar $ actChan act
  case ind of
    Just i  -> do
      dat <- envExpr $ actData act
      return act { actData = dat, actChanIndex = i }
    Nothing -> throwError $ SimpleError "var not in scope"
envAction act@AInput  {} = do
  chanInd <- lookupVar $ actChan act
  case chanInd of
    Nothing -> throwError $ SimpleError "var not in scope"
    Just i  -> do
      j <- allocVar $ actBind act
      return act { actChanIndex = i, actBindIndex = j }
envAction act@ANew    {} = do
  i <- allocVar $ actBind act
  return act { actBindIndex = i }
envAction act@ALet    {} = do
  i <- allocVar $ actBind act
  val <- envExpr $ actVal act
  return act { actBindIndex = i, actVal = val }
envAction act@ASpawn  {} = do
  args <- mapM envExpr $ actArgs act
  return act { actArgs = args }
envAction act@APrim   {} = do
  args <- mapM envExpr $ actArgs act
  return $ act { actArgs = args }

envExpr:: Expr -> EnvM Expr
envExpr e@ETuple {} = do
  envVals <- mapM envExpr $ exprVals e
  return $ e { exprVals = envVals }
envExpr e@EVar   {} = do
  ind <- lookupVar $ exprVar e
  case ind of
    Just index -> return $ e { exprIndex = index }
    Nothing    -> throwError $ SimpleError "var not in scope"
envExpr e@EPrim  {} = do
  args <- mapM envExpr $ exprArgs e
  return $ e { exprArgs = args }
envExpr e@EAnd   {} = do
  left  <- envExpr (exprLeft e)
  right <- envExpr (exprRight e)
  return $ e { exprLeft = left, exprRight = right }
envExpr e@EOr    {} = do
  left  <- envExpr (exprLeft e)
  right <- envExpr (exprRight e)
  return $ e { exprLeft = left, exprRight = right }
envExpr e = return e

