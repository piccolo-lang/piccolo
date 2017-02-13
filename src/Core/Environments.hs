{-|
Module         : Core.Environments
Description    : Indexes of variables computation pass
Stability      : experimental

In runtime, process environment have a fixed size,
and each variable is called using an index in this environment.
This module computes these indexes and decorates the core AST with them.
__TODO__: rewrite the computingEnvPass function (it's ugly...)
-}
module Core.Environments
  ( computingEnvPass
  )
where

import Core.AST
import Errors

import Data.List
import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.State


type DefsEnv = Map.Map String Int

data Environment = Environment { defsSizes     :: DefsEnv
                               , lexicalEnv    :: [String]
                               , maxCalledSize :: Int
                               , maxChoiceSize :: Int
                               }

type EnvM a = ExceptT PiccError (State Environment) a

lookupSizeOfDef :: String -> EnvM Int
lookupSizeOfDef name = do
  Environment { defsSizes = defs } <- get
  return $ defs Map.! name

registerACall :: String -> EnvM ()
registerACall name = do
  size <- lookupSizeOfDef name
  env  <- get
  let maxCalled = maxCalledSize env
  when (maxCalled < size) $ put env { maxCalledSize = size }

lookupVar :: String -> EnvM (Maybe Int)
lookupVar v = do
  Environment { lexicalEnv = env } <- get
  return $ elemIndex v env

addVar :: String -> EnvM Int
addVar v = do
  ind <- lookupVar v
  case ind of
    Just i  -> return i
    Nothing -> do
      env <- get
      let lexEnv = lexicalEnv env
      put env { lexicalEnv = lexEnv ++ [v] }
      return $ length lexEnv

beforeDefComputation :: [String] -> EnvM ()
beforeDefComputation args = do
  env <- get
  put env { lexicalEnv = args, maxCalledSize = 0, maxChoiceSize = 0 }

afterDefComputation :: EnvM Int
afterDefComputation = do
  env <- get
  let envSize = length $ lexicalEnv env
  let calSize = maxCalledSize env
  return $ maximum [envSize, calSize]

registerDefSize :: String -> Int -> EnvM ()
registerDefSize name size = do
  env <- get
  let defs = defsSizes env
  put env { defsSizes = Map.insert name size defs }


-- | The 'computingEnvPass' function computes environment that are needed to
-- address variable with a simple integer at the runtime. It also decorates
-- the AST with those indexes.
computingEnvPass :: Modul -> Either PiccError Modul
computingEnvPass m = loopUntilFix m $ Map.fromList (map extractSize (modDefs m))
  where extractSize def = let i = defEnvSize def in
                          let n = defName def in
                          if i < 0 then (n, 0) else (n, i)
        loopUntilFix modul defSizes = let (result, env) = runState (runExceptT (envModule modul)) (Environment defSizes [] 0 0) in
                                    case result of
                                      Left err     -> Left err
                                      Right modul' -> if defSizes == defsSizes env
                                                      then Right modul'
                                                      else loopUntilFix modul' (defsSizes env)

envModule :: Modul -> EnvM Modul
envModule m = do
  defs <- mapM envDefinition $ modDefs m
  return $ m { modDefs = defs }

envDefinition :: Definition -> EnvM Definition
envDefinition def = do
  beforeDefComputation $ map (\(x,_,_) -> x) (defParams def)
  proc <- envProcess $ defBody def
  size <- afterDefComputation
  registerDefSize (defName def) size
  return $ def { defBody = proc, defEnvSize = size }

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
  registerACall name
  args <- mapM envExpr $ procArgs proc
  return proc { procArgs = args }

envBranch :: Branch -> EnvM Branch
envBranch br@BTau    {} = do
  gard <- envExpr $ brGuard br
  cont <- envProcess $ brCont br
  return br { brGuard = gard, brCont = cont }
envBranch br@BOutput {} = do
  gard    <- envExpr $ brGuard br
  chanInd <- lookupVar $ brChan br
  case chanInd of
    Just i  -> do
      dat  <- envExpr $ brData br
      cont <- envProcess $ brCont br
      return br { brGuard = gard, brChanIndex = i, brData = dat, brCont = cont }
    Nothing -> throwError $ OtherError "var not in scope"
envBranch br@BInput  {} = do
  gard    <- envExpr $ brGuard br
  chanInd <- lookupVar $ brChan br
  case chanInd of
    Nothing -> throwError $ OtherError "var not in scope"
    Just i  -> do
      j <- addVar $ brBind br
      cont <- envProcess $ brCont br
      return br { brGuard = gard, brChanIndex = i, brBindIndex = j, brCont = cont }

envAction :: Action -> EnvM Action
envAction act@AOutput {} = do
  ind <- lookupVar $ actChan act
  case ind of
    Just i  -> do
      dat <- envExpr $ actData act
      return act { actData = dat, actChanIndex = i }
    Nothing -> throwError $ OtherError "var not in scope"
envAction act@AInput  {} = do
  chanInd <- lookupVar $ actChan act
  case chanInd of
    Nothing -> throwError $ OtherError "var not in scope"
    Just i  -> do
      j <- addVar $ actBind act
      return act { actChanIndex = i, actBindIndex = j }
envAction act@ANew    {} = do
  i <- addVar $ actBind act
  return act { actBindIndex = i }
envAction act@ALet    {} = do
  i <- addVar $ actBind act
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
    Nothing    -> throwError $ OtherError "var not in scope"
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

