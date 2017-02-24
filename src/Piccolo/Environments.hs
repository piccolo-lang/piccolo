{-|
Module         : Piccolo.Environments
Description    : Indexes of variables computation pass
Stability      : experimental

The Piccolo runtime is stackless, and values environments have fixed size.
The goal of this pass is to compute the size of such environments and to
attribute an index to each variable in the AST.
Moreover, the max number of branches in a process definition is needed at
runtime and is computed at the same time.
Due to nested process calls, those two computations are done by iteratively
computing the max size and looking for a fixpoint of this function.
-}

module Piccolo.Environments
  ( computingEnvPass
  )
where

import Piccolo.AST
import Piccolo.Errors

import Data.List
import Data.Maybe
import Control.Monad.Except
import Control.Monad.State
import Control.Arrow ((&&&))


data Environment = Environment { lexicalSizes :: [(String, Int)]
                                   -- current lexical env size per def
                               , choicesSizes :: [(String, Int)]
                                   -- current choice env max size per def
                               , maxLexicalSize :: Int
                                   -- max lexical size of called defs
                               , maxChoicesSize :: Int
                                   -- max choice max size of called defs
                               , currentLexicalEnv :: [String]
                                   -- list of variables in scope
                               }

-- | Environment initializer. Should be called once per module iteration.
initModEnv :: Modul -> Environment
initModEnv m = Environment lexSizes chcSizes 0 0 []
  where lexSizes = map (defName &&& defLexicalEnvSize) $ modDefs m
        chcSizes = map (defName &&& defChoiceMaxSize)  $ modDefs m

type EnvM a = ExceptT PiccError (State Environment) a

-- | Environment initialize. Should be called once per definition iteration.
initDefEnv :: [String] -> EnvM ()
initDefEnv args = do
  env <- get
  put env { maxLexicalSize = 0
          , maxChoicesSize = 0
          , currentLexicalEnv = args
          }

finalEnvSizes :: EnvM (Int, Int)
finalEnvSizes = do
  env <- get
  let x = length $ currentLexicalEnv env
  let y = maxLexicalSize env
  let z = maxChoicesSize env
  return (maximum [x, y], z)

lookupLexVar :: String -> EnvM (Maybe Int)
lookupLexVar v = do
  env <- get
  return $ elemIndex v (currentLexicalEnv env)

addLexVar :: String -> EnvM Int
addLexVar v = do
  ind <- lookupLexVar v
  case ind of
    Just i  -> return i
    Nothing -> do
      env <- get
      let lexEnv = currentLexicalEnv env
      put env { currentLexicalEnv = lexEnv ++ [v] }
      return $ length lexEnv

updateLexSize :: String -> EnvM ()
updateLexSize name = do
  env <- get
  let x = maxLexicalSize env
  let y = fromJust $ lookup name (lexicalSizes env)
  when (y > x) $ put env { maxLexicalSize = y }

updateChcSize :: String -> EnvM ()
updateChcSize name = do
  env <- get
  let x = maxChoicesSize env
  let y = fromJust $ lookup name (choicesSizes env)
  when (y > x) $ put env { maxChoicesSize = y }

registerChcSize :: Int -> EnvM ()
registerChcSize y = do
  env <- get
  let x = maxChoicesSize env
  when (y > x) $ put env { maxChoicesSize = y }


-- | 'convergeEnvs' takes a stream of results of successive envs computation,
-- and returns a Modul when a fixpoint is achieved (with respect to
-- potential errors
convergeEnvs :: [Either PiccError Modul] -> Either PiccError Modul
convergeEnvs (x@(Left _) : _)     = x
convergeEnvs (_ : y@(Left _) : _) = y
convergeEnvs (Right x : ys @ (Right y : _)) =
  if xLexEnvSizes == yLexEnvSizes && xChcMaxSizes == yChcMaxSizes then
    Right x
  else
    convergeEnvs ys
  where xLexEnvSizes = map defLexicalEnvSize $ modDefs x
        yLexEnvSizes = map defLexicalEnvSize $ modDefs y
        xChcMaxSizes = map defChoiceMaxSize  $ modDefs x
        yChcMaxSizes = map defChoiceMaxSize  $ modDefs y
convergeEnvs _ = Left $ OtherError "should not happen"

computingEnvPass :: Modul -> Either PiccError Modul
computingEnvPass = convergeEnvs . iterate f . Right
  where f em = em >>= \m -> evalState (runExceptT (envModule m)) (initModEnv m)

envModule :: Modul -> EnvM Modul
envModule m = do
  defs <- mapM envDefinition $ modDefs m
  return $ m { modDefs = defs }

envDefinition :: Definition -> EnvM Definition
envDefinition def = do
  initDefEnv $ map (\(x,_,_) -> x) (defParams def)
  proc <- envProcess $ defBody def
  (lexSize, chcSize) <- finalEnvSizes
  return def { defBody = proc
             , defLexicalEnvSize = lexSize
             , defChoiceMaxSize  = chcSize
             }

envProcess :: Process -> EnvM Process
envProcess proc@PEnd    {} = return proc
envProcess proc@PPrefix {} = do
  pref <- envAction $ procPref proc
  cont <- envProcess $ procCont proc
  return proc { procPref = pref, procCont = cont }
envProcess proc@PChoice { procBranches = brs } = do
  registerChcSize $ length brs
  branches <- mapM envBranch brs
  return proc { procBranches = branches }
envProcess proc@PCall { procName = name } = do
  updateLexSize name
  updateChcSize name
  args <- mapM envExpr $ procArgs proc
  return proc { procArgs = args }

envBranch :: Branch -> EnvM Branch
envBranch br@BTau    {} = do
  gard <- envExpr $ brGuard br
  cont <- envProcess $ brCont br
  return br { brGuard = gard, brCont = cont }
envBranch br@BOutput {} = do
  gard    <- envExpr $ brGuard br
  chanInd <- lookupLexVar $ brChan br
  case chanInd of
    Just i -> do
      dat  <- envExpr $ brData br
      cont <- envProcess $ brCont br
      return br { brGuard = gard, brChanIndex = i, brData = dat, brCont = cont}
    Nothing -> throwError $ VarNotFoundError (brChan br) (localize br)
envBranch br@BInput {} = do
  gard    <- envExpr $ brGuard br
  chanInd <- lookupLexVar $ brChan br
  case chanInd of
    Just i  -> do
      j    <- addLexVar $ brBind br
      cont <- envProcess $ brCont br
      return br { brGuard = gard, brChanIndex = i, brBindIndex = j, brCont = cont }
    Nothing -> throwError $ VarNotFoundError (brChan br) (localize br)

envAction :: Action -> EnvM Action
envAction act@AOutput {} = do
  ind <- lookupLexVar $ actChan act
  case ind of
    Just i  -> do
      dat <- envExpr $ actData act
      return act { actData = dat, actChanIndex = i }
    Nothing -> throwError $ VarNotFoundError (actChan act) (localize act)
envAction act@AInput  {} = do
  chanInd <- lookupLexVar $ actChan act
  case chanInd of
    Just i -> do
      j <- addLexVar $ actBind act
      return act { actChanIndex = i, actBindIndex = j }
    Nothing -> throwError $ VarNotFoundError (actChan act) (localize act)
envAction act@ANew    {} = do
  i <- addLexVar $ actBind act
  return act { actBindIndex = i }
envAction act@ALet    {} = do
  i <- addLexVar $ actBind act
  return act { actBindIndex = i }
envAction act@ASpawn  {} = do
  args <- mapM envExpr $ actArgs act
  return act { actArgs = args }
envAction act@APrim   {} = do
  args <- mapM envExpr $ actArgs act
  return act { actArgs = args }

envExpr :: Expr -> EnvM Expr
envExpr e@ETuple {} = do
  envVals <- mapM envExpr $ exprVals e
  return $ e { exprVals = envVals }
envExpr e@EVar   {} = do
  ind <- lookupLexVar $ exprVar e
  case ind of
    Just index -> return e { exprIndex = index }
    Nothing    -> throwError $ VarNotFoundError (exprVar e) (localize e)
envExpr e@EPrim  {} = do
  args <- mapM envExpr $ exprArgs e
  return e { exprArgs = args }
envExpr e@EAnd   {} = do
  left  <- envExpr $ exprLeft e
  right <- envExpr $ exprRight e
  return e { exprLeft = left, exprRight = right }
envExpr e@EOr    {} = do
  left  <- envExpr $ exprLeft e
  right <- envExpr $ exprRight e
  return e { exprLeft = left, exprRight = right }
envExpr e = return e
