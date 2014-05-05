module Middle.Typing (typingPass) where

import Front.AST
import Front.ASTUtils
import PiccError
import Primitives

import Control.Monad.Error
import Control.Monad.State
import qualified Data.Map as Map
import Data.List (find)

type TypingDefsEnv = [Definition]
type TypingVarsEnv = Map.Map String TypeExpr
type TypingEnv = (TypingDefsEnv, TypingVarsEnv)

type TypingM a = ErrorT PiccError (State TypingEnv) a

putVarType :: String -> TypeExpr -> TypingM ()
putVarType v t = do
  (defs,vars) <- lift $ get
  lift $ put (defs, Map.insert v t vars)

getVarType :: String -> TypingM TypeExpr
getVarType v = do
  (defs,vars) <- lift $ get
  case Map.lookup v vars of
    Just t  -> return t
    Nothing -> throwError (SimpleError $ "var " ++ v ++ " not found")

typingPass :: ModuleDef -> Either PiccError ModuleDef
typingPass mDef = evalState (runErrorT tChecked) initEnv
  where initEnv  = (moduleDefs mDef, Map.empty)
        tChecked = tcModule mDef

tcModule :: ModuleDef -> TypingM ModuleDef
tcModule mDef = do
  defs <- mapM tcDefinition $ moduleDefs mDef
  return $ mDef { moduleDefs = defs }

tcDefinition :: Definition -> TypingM Definition
tcDefinition def = do
  let vars = Map.fromList $ map (\(v,t,_) -> (v,t)) (defParams def)
  proc <- tcProcess $ defBody def
  return $ def { defBody = proc }

tcProcess :: Process -> TypingM Process
tcProcess proc@(PEnd {}) = return proc
tcProcess proc@(PChoice {}) = do
  branches <- mapM tcBranch $ procBranches proc
  return $ proc { procBranches = branches }
tcProcess proc@(PCall { procName = name }) = do
  (defs,_) <- lift $ get
  def <- case find (\d@(Definition { defName = n }) -> n == name) defs of
           Nothing -> throwError (SimpleError $ "proc def " ++ name ++ " not found")
           Just d  -> return d
  let typParams = map (\(_,t,_) -> t) $ defParams def
  args <- mapM tcValue $ procArgs proc
  let typArgs   = map valTyp args
  if length typParams /= length typArgs
    then throwError (SimpleError $ "bad arity when calling " ++ name ++ " proc")
    else return ()
  forM_ (zip typParams typArgs) (\(p,a) -> do
    if p /= a
      then throwError (SimpleError $ "bad type in proc")
      else return ())
  return $ proc { procArgs = args }

tcBranch :: Branch -> TypingM Branch
tcBranch branch = do
  guard  <- tcValue   $ bGuard branch
  action <- tcAction  $ bAction branch
  cont   <- tcProcess $ bCont branch
  return $ branch { bGuard = guard, bAction = action, bCont = cont }

tcAction :: Action -> TypingM Action
tcAction act@(ATau {}) = return act
tcAction act@(AOutput { actChan = actChan, actData = actData }) = do
  chanTyp <- getVarType actChan
  dat     <- tcValue actData
  if chanTyp /= TChannel (valTyp dat) noLoc
    then throwError (SimpleError "bad channel type for output")
    else return ()
  return $ act { actData = dat }
tcAction act@(AInput {}) = do
  chanTyp <- getVarType $ actChan act
  dataTyp <- case chanTyp of
    TChannel { typExpr = typExpr } -> return typExpr
    _ -> throwError (SimpleError $ "input on a variable that is not a channel")
  putVarType (actBind act) dataTyp
  return act
tcAction act@(ANew {}) = do
  case actTyp act of
    TChannel {} -> return ()
    _ -> throwError (SimpleError $ "creating with \"new\" a var that is not a channel")
  putVarType (actBind act) (actTyp act)
  return act
tcAction act@(ALet {}) = do
  val <- tcValue $ actVal act
  if actTyp act /= valTyp val
    then throwError (SimpleError $ "wrong type in let")
    else return ()
  return $ act { actVal = val }
tcAction act@(ASpawn {}) = error "TODO"
tcAction act@(APrim { actModule = m, actName = n }) = do
  (_, typParams) <- case Map.lookup (m,n) primTypes of
    Nothing -> throwError (SimpleError $ "primitive not found " ++ m ++ "#" ++ n)
    Just t  -> return t
  args <- mapM tcValue $ actArgs act
  let typArgs = map valTyp args
  if length typParams /= length typArgs
    then throwError (SimpleError $ "bad arity when calling " ++ m ++ "#" ++ n ++ " primitive")
    else return ()
  forM_ (zip typParams typArgs) (\(p,a) -> do
    if p /= a
      then throwError (SimpleError $ "bad type in prim")
      else return ())
  return $ act { actArgs = args }

tcValue :: Value -> TypingM Value
tcValue val@(VTrue {})   = return $ val { valTyp = TAtom TBool noLoc }
tcValue val@(VFalse {})  = return $ val { valTyp = TAtom TBool noLoc }
tcValue val@(VInt {})    = return $ val { valTyp = TAtom TInt noLoc }
tcValue val@(VString {}) = return $ val { valTyp = TAtom TString noLoc }
tcValue val@(VTuple { valVals = vs }) = do
  typedVals <- mapM tcValue vs
  let tuplTyp = TTuple (map valTyp typedVals) noLoc
  return $ val { valVals = typedVals, valTyp = tuplTyp }
tcValue val@(VVar { valVar = v }) = do
  typ <- getVarType v
  return $ val { valTyp = typ }
tcValue val@(VPrim { valModule = m, valName = n }) = do
  (typRet, typParams) <- case Map.lookup (m,n) primTypes of
    Nothing -> throwError (SimpleError $ "primitive not found " ++ m ++ "#" ++ n)
    Just t  -> return t
  args <- mapM tcValue $ valArgs val
  let typArgs = map valTyp args
  if length typParams /= length typArgs
    then throwError (SimpleError $ "bad arity when calling " ++ m ++ "#" ++ n ++ " primitive")
    else return ()
  forM_ (zip typParams typArgs) (\(p,a) -> do
    if p /= a
      then throwError (SimpleError $ "bad type in prim")
      else return ())
  return $ val { valArgs = args, valTyp = typRet }
