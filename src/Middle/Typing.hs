{-|
Module         : Middle.Typing
Description    : Typing module for the piccolo language
Stability      : experimental

Since types are explicitly written in the code when introducing a new variable (via a "let", a "new",
or process definition parameters), the typing pass of the piccolo compiler only check types through
a traversal of the AST and tag each value with its types for compilation pass.
-}
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

getVarType :: String -> Location -> TypingM TypeExpr
getVarType v loc = do
  (defs,vars) <- lift $ get
  case Map.lookup v vars of
    Just t  -> return t
    Nothing -> throwError $ VarNotFoundError v loc

-- | The 'typingPass' function typechecks each definition of a module,
-- and return either a 'PiccError' or the type annotated version of this module.
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
  mapM_ (\(v,t,_) -> putVarType v t) (defParams def)
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
           Nothing -> throwError $ DefNotFoundError name (localize proc)
           Just d  -> return d
  let typParams = map (\(_,t,_) -> t) $ defParams def
  args <- mapM tcValue $ procArgs proc
  let typArgs   = map valTyp args
  if length typParams /= length typArgs
    then throwError $ ArityError name (localize proc) (length typParams) (length typArgs)
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
  chanTyp <- getVarType actChan (localize act)
  dat     <- tcValue actData
  if chanTyp /= TChannel (valTyp dat) noLoc
    then throwError (SimpleError "bad channel type for output")
    else return ()
  return $ act { actData = dat }
tcAction act@(AInput {}) = do
  chanTyp <- getVarType (actChan act) (localize act)
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
  putVarType (actBind act) (actTyp act)
  return $ act { actVal = val }
tcAction act@(ASpawn { actName = name }) = do
  (defs,_) <- lift $ get
  def <- case find (\d@(Definition { defName = n }) -> n == name) defs of
           Nothing -> throwError (SimpleError $ "proc def " ++ name ++ " not found")
           Just d  -> return d
  let typParams = map (\(_,t,_) -> t) $ defParams def
  args <- mapM tcValue $ actArgs act
  let typArgs   = map valTyp args
  if length typParams /= length typArgs
    then throwError $ ArityError name (localize act) (length typParams) (length typArgs)
    else return ()
  forM_ (zip typParams typArgs) (\(p,a) -> do
    if p /= a
      then throwError (SimpleError $ "bad type in spawn")
      else return ())
  return $ act { actArgs = args }
tcAction act@(APrim { actModule = m, actName = n }) = do
  (_, typParams) <- case Map.lookup (m,n) primTypes of
    Nothing -> throwError $ PrimNotFoundError ("#" ++ m ++ ":" ++ n) (localize act)
    Just t  -> return t
  args <- mapM tcValue $ actArgs act
  if length typParams /= length args
    then throwError $ ArityError (m ++ "#" ++ n) (localize act) (length typParams) (length args)
    else return ()
  forM_ (zip typParams args) (\(p,a) -> do
    if p /= valTyp a
      then throwError $ TypingError (show a) (localize a) p (valTyp a)
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
  typ <- getVarType v (localize val)
  return $ val { valTyp = typ }
tcValue val@(VPrim { valModule = m, valName = n }) = do
  (typRet, typParams) <- case Map.lookup (m,n) primTypes of
    Nothing -> throwError $ PrimNotFoundError ("#" ++ m ++ ":" ++ n) (localize val)
    Just t  -> return t
  args <- mapM tcValue $ valArgs val
  let typArgs = map valTyp args
  if length typParams /= length typArgs
    then throwError $ ArityError (m ++ "#" ++ n) (localize val) (length typParams) (length typArgs)
    else return ()
  forM_ (zip typParams typArgs) (\(p,a) -> do
    if p /= a
      then throwError (SimpleError $ "bad type in prim")
      else return ())
  return $ val { valArgs = args, valTyp = typRet }
