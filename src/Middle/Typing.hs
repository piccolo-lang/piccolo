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
  (defs,vars) <- lift get
  lift $ put (defs, Map.insert v t vars)

getVarType :: String -> Location -> TypingM TypeExpr
getVarType v loc = do
  (_,vars) <- lift get
  case Map.lookup v vars of
    Just t  -> return t
    Nothing -> throwError $ VarNotFoundError v loc

-- | The 'typingPass' function typechecks each definition of a module,
-- and return either a 'PiccError' or the type annotated version of this module.
typingPass :: Modul -> Either PiccError Modul
typingPass m = evalState (runErrorT tChecked) initEnv
  where initEnv  = (modDefs m, Map.empty)
        tChecked = tcModul m


tcModul :: Modul -> TypingM Modul
tcModul m = do
  defs <- mapM tcDefinition $ modDefs m
  return m { modDefs = defs }

tcDefinition :: Definition -> TypingM Definition
tcDefinition def = do
  mapM_ (\(v,t,_) -> putVarType v t) (defParams def)
  proc <- tcProcess $ defBody def
  return def { defBody = proc }

tcProcess :: Process -> TypingM Process

tcProcess proc@PEnd    {} = return proc

tcProcess proc@PPrefix {} = do
  action   <- tcAction $ procPref proc
  cont     <- tcProcess $ procCont proc
  return proc { procPref = action, procCont = cont }

tcProcess proc@PChoice {} = do
  branches <- mapM tcBranch $ procBranches proc
  return proc { procBranches = branches }

tcProcess proc@PCall { procName = name } = do
  (defs, _) <- lift get
  def <- case find (\d -> defName d == name) defs of
           Nothing -> throwError $ DefNotFoundError name (localize proc)
           Just d  -> return d
  let typParams = map (\(_,t,_) -> t) $ defParams def
  args <- mapM tcExpr $ procArgs proc
  let typOfArgs = map exprTyp args
  when (length typParams /= length typOfArgs) $
    throwError $ ArityError name (localize proc) (length typParams) (length typOfArgs)
  forM_ (zip typParams typOfArgs) (\(p,a) -> when (p /= a) $ throwError (SimpleError "bad type in proc"))
  return proc { procArgs = args }

tcBranch :: Branch -> TypingM Branch

tcBranch br@BTau    {} = do
  gard <- tcExpr $ brGuard br
  cont <- tcProcess $ brCont br
  return br { brGuard = gard, brCont = cont }

tcBranch br@BOutput {} = do
  gard    <- tcExpr $ brGuard br
  chanTyp <- getVarType (brChan br) (localize br)
  dat     <- tcExpr $ brData br
  when (chanTyp /= TChannel (exprTyp dat) noLoc) $
    throwError (SimpleError "bad channel type for output")
  cont    <- tcProcess $ brCont br
  return br { brGuard = gard, brData = dat, brCont = cont }

tcBranch br@BInput {} = do
  gard    <- tcExpr $ brGuard br
  chanTyp <- getVarType (brChan br) (localize br)
  dataTyp <- case chanTyp of
    typ@TChannel {} -> return $ typExpr typ
    _ -> throwError (SimpleError "input on a variable that is not a channel")
  putVarType (brBind br) dataTyp
  cont    <- tcProcess $ brCont br
  return br { brGuard = gard, brBindTyp = dataTyp, brCont = cont }

tcAction :: Action -> TypingM Action

tcAction act@AOutput {} = do
  chanTyp <- getVarType (actChan act) (localize act)
  dat     <- tcExpr (actData act)
  when (chanTyp /= TChannel (exprTyp dat) noLoc) $
    throwError (SimpleError "bad channel type for output")
  return act { actData = dat }

tcAction act@AInput  {} = do
  chanTyp <- getVarType (actChan act) (localize act)
  dataTyp <- case chanTyp of
    typ@TChannel {} -> return $ typExpr typ
    _ -> throwError (SimpleError "input on a variable that is not a channel")
  putVarType (actBind act) dataTyp
  return act { actBindTyp = dataTyp }

tcAction act@ANew    {} = do
  case actTyp act of
    TChannel {} -> return ()
    _ -> throwError (SimpleError "creating with \"new\" a var that is not a channel")
  putVarType (actBind act) (actTyp act)
  return act

tcAction act@ALet    {} = do
  val <- tcExpr $ actVal act
  when (actTyp act /= exprTyp val) $
    throwError (SimpleError "wrong type in let")
  putVarType (actBind act) (actTyp act)
  return act { actVal = val }

tcAction act@ASpawn { actName = name} = do
  (defs,_) <- lift get
  def <- case find (\d -> defName d == name) defs of
           Nothing -> throwError (SimpleError $ "proc def " ++ name ++ " not found")
           Just d  -> return d
  let typParams = map (\(_,t,_) -> t) $ defParams def
  args <- mapM tcExpr $ actArgs act
  let typOfArgs   = map exprTyp args
  when (length typParams /= length typOfArgs) $
    throwError $ ArityError name (localize act) (length typParams) (length typOfArgs)
  forM_ (zip typParams typOfArgs) (\(p,a) ->
    when (p /= a) $ throwError (SimpleError "bad type in spawn"))
  return act { actArgs = args }

tcAction act@APrim { actModule = m, actName = n } = do
  (_, typParams) <- case Map.lookup (m,n) primTypes of
    Nothing -> throwError $ PrimNotFoundError ("#" ++ m ++ ":" ++ n) (localize act)
    Just t  -> return t
  args <- mapM tcExpr $ actArgs act
  when (length typParams /= length args) $
    throwError $ ArityError (m ++ "#" ++ n) (localize act) (length typParams) (length args)
  forM_ (zip typParams args) (\(p,a) -> when (p /= exprTyp a) $
    throwError $ TypingError (show a) (localize a) p (exprTyp a))
  return act { actArgs = args }

tcExpr :: Expr -> TypingM Expr
tcExpr e@ETrue   {} = return e { exprTyp = TAtom TBool noLoc }
tcExpr e@EFalse  {} = return e { exprTyp = TAtom TBool noLoc }
tcExpr e@EInt    {} = return e { exprTyp = TAtom TInt noLoc }
tcExpr e@EString {} = return e { exprTyp = TAtom TString noLoc }
tcExpr e@ETuple  { exprVals = vs } = do
  typedVals <- mapM tcExpr vs
  let tuplTyp = TTuple (map exprTyp typedVals) noLoc
  return e { exprVals = typedVals, exprTyp = tuplTyp }
tcExpr e@EVar { exprVar = v } = do
  typ <- getVarType v (localize e)
  return e { exprTyp = typ }
tcExpr e@EPrim   { exprModule = m, exprName = n } = do
  (typOfRet, typParams) <- case Map.lookup (m,n) primTypes of
    Nothing -> throwError $ PrimNotFoundError ("#" ++ m ++ ":" ++ n) (localize e)
    Just t  -> return t
  args <- mapM tcExpr $ exprArgs e
  let typOfArgs = map exprTyp args
  when (length typParams /= length typOfArgs) $
    throwError $ ArityError (m ++ "#" ++ n) (localize e) (length typParams) (length typOfArgs)
  forM_ (zip typParams typOfArgs) (\(p,a) -> when (p /= a) $
    throwError (SimpleError "bad type in prim"))
  return e { exprArgs = args, exprTyp = typOfRet }
tcExpr e@EAnd    {} = do
  tLeft  <- tcExpr (exprLeft e)
  tRight <- tcExpr (exprRight e)
  when (not (isBool (exprTyp tLeft))) $
    throwError $ SimpleError "bad type in and (left)"
  when (not (isBool (exprTyp tRight))) $
    throwError $ SimpleError "bad type in and (right)"
  return $ e { exprLeft = tLeft, exprRight = tRight }
tcExpr e@EOr     {} = do
  tLeft  <- tcExpr (exprLeft e)
  tRight <- tcExpr (exprRight e)
  when (not (isBool (exprTyp tLeft))) $
    throwError $ SimpleError "bad type in or(left)"
  when (not (isBool (exprTyp tRight))) $
    throwError $ SimpleError "bad type in or (right)"
  return $ e { exprLeft = tLeft, exprRight = tRight }
