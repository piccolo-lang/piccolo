{-|
Module         : Middle.Compilation
Description    : Compilation module
Stability      : experimental

This module contains the compilation pass. It transforms a piccolo AST to a sequential AST.
-}
module Middle.Compilation (compilePass) where

import PiccError
import Front.AST
import Front.ASTUtils
import Back.SeqASTUtils
import Back.SeqAST ( DefName(..)
                   , PrimName(..)
                   , EvalfuncName(..)
                   , Instr(DefFunction, EvalFunction,
                           Nop, Return, ReturnVal, Goto, Switch, Case, CaseAndLabel)
                   , Type (..)
                   , BExpr (Not)
                   )

import Data.List (delete)
import Control.Monad.Error
import Control.Monad.State


data CompState = CompState { labelCount    :: Int
                           , currentModule :: String
                           , evalFuncCount :: Int
                           , evalFuncs     :: [Instr]
                           }

type CompilingM a = ErrorT PiccError (State CompState) a

dEntry :: Int
dEntry = 0

resetLabCount :: CompilingM ()
resetLabCount = do
  st <- get
  put st { labelCount = dEntry + 1 }

genLabel :: CompilingM Int
genLabel = do
  st <- get
  let l = labelCount st
  put st { labelCount = l + 1 }
  return l

registerEvalFunc :: Instr -> CompilingM EvalfuncName
registerEvalFunc expr = do
  st <- get
  let n  = evalFuncCount st
  let fs = evalFuncs st
  let fName = EvalfuncName n
  let f = EvalFunction fName (pt, PiThreadType) expr
  put st { evalFuncCount = n + 1, evalFuncs = fs ++ [f] }
  return fName


-- | The 'compilePass' monad run the piccolo AST compilation to sequential AST
compilePass :: Modul -> Either PiccError Instr
compilePass m = evalState (runErrorT instr) initEnv
  where instr    = compileModul m
        initEnv  = CompState (dEntry + 1) "" 1 []


compileModul :: Modul -> CompilingM Instr
compileModul m = do
  st <- get
  put st { currentModule = modName m }
  defs <- forM (modDefs m) $ \def -> compileDefinition def
  CompState { evalFuncs = fs } <- get
  let defs' = foldr (#) Nop defs
  return $ foldr (#) defs' fs

compileDefinition :: Definition -> CompilingM Instr
compileDefinition def = do
  resetLabCount
  CompState { currentModule = m } <- get
  let name = DefName m (defName def)
  procInstrs <- compileProcess (defBody def)
  return $ DefFunction name((scheduler, SchedulerType), (pt, PiThreadType))
           (
             Switch pt_pc
             (
               Case dEntry #
               procInstrs
             )
           )

compileProcess :: Process -> CompilingM Instr
compileProcess proc@PEnd {} = do
  return $ comment (strSExpr [] proc) #
           processEnd(pt, statusEnded) #
           Return
           
compileProcess proc@PPrefix {} = do
  pref <- compileAction $ procPref proc
  cont <- compileProcess $ procCont proc
  return $ pref # cont

compileProcess proc@PChoice {} = do
  let n = length (procBranches proc)
  startChoice <- genLabel
  choiceConts <- replicateM n genLabel
  loop1 <- forM (zip ([0..]::[Int]) (procBranches proc)) $ \(i, br) -> do
    expr <- compileExpr (brGuard br)
    act  <- compileBranchAction i br
    return $ expr #
             (pt_enabled i) <-- unboxBoolValue(pt_val) #
             ifthenelse (pt_enabled i)
             (
               act #
               ifthenelse (tryResult =:= tryResultDisabled)
               (
                 (pt_enabled i) <---- 0 #
                 incr nbDisabled
               )
               (
                 ifthenelse (tryResult =:= tryResultAbort)
                 (
                   releaseAllChannels(chans) #
                   pt_pc <---- startChoice #
                   processYield(pt, scheduler) #
                   Return
                 )
                 (
                   ifthen (tryResult =:= tryResultEnabled)
                   (
                     releaseAllChannels(chans) #
                     decr pt_fuel #
                     ifthen (pt_fuel =:== 0)
                     (
                       pt_pc <---- (choiceConts !! i) #
                       processYield(pt, scheduler) #
                       Return
                     ) #
                     Goto (choiceConts !! i)
                   )
                 )
               )
             )
             (
               incr nbDisabled
             )
  loop2 <- forM (zip ([0..]::[Int]) (procBranches proc)) $ \(i, br) -> do
    case br of
      BOutput { brChanIndex = c } -> do
        instrs   <- compileExpr (brData br)
        evalFunc <- registerEvalFunc $ instrs # ReturnVal
        return $ ifthen (pt_enabled i)
                 (
                   registerOutputCommitment(pt, unboxChannelValue(pt_env c), evalFunc, choiceConts !! i)
                 )
      BInput { brChanIndex = c, brBindIndex = x } -> do
        return $ ifthen (pt_enabled i)
                 (
                   registerInputCommitment(pt, unboxChannelValue(pt_env c), x, choiceConts !! i)
                 )
      _ -> return Nop
  loop3 <- forM (zip ([0..]::[Int]) (procBranches proc)) $ \(i, br) -> do
    cont <- compileProcess (brCont br)
    return $ CaseAndLabel (choiceConts !! i) #
             cont
  return $ Case startChoice #
           (begin $ var tryResult TryResultEnumType #
                    var chans ChannelArrayType #
                    chans <-- createEmptyKnownSet() #
                    var nbDisabled IntType #
                    nbDisabled <---- 0 #
                    var nbChans IntType #
                    nbChans <---- 0 #
                    foldr (#) Nop loop1 #
                    ifthen (nbDisabled =:== n)
                    (
                      releaseAllChannels(chans) #
                      processEnd(pt, statusBlocked)
                    ) #
                    foldr (#) Nop loop2 #
                    acquire(pt_lock) #
                    releaseAllChannels(chans) #
                    processWait(pt, scheduler) #
                    Return #
                    foldr (#) Nop loop3
           )

compileProcess proc@PCall {} = do
  loop1 <- forM (zip ([1..]::[Int]) (procArgs proc)) $ \(i, arg) -> do
    let vi = v i
    expr <- compileExpr arg
    return $ expr #
             var vi PiValueType #
             vi <-- pt_val
  loop2 <- forM (zip ([1..]::[Int]) (procArgs proc)) $ \(i, arg) -> do
    let vi = v i
    return $ (pt_env (i-1)) <-- vi #
             (case exprTyp arg of
                TChannel {} -> knowRegister(pt_knows, vi)
                _           -> Nop
             )
  CompState { currentModule = currentModName } <- get
  let name | null (procModule proc) = (delete '/' $ currentModName) ++ "_" ++ procName proc
           | otherwise              = (delete '/' $ procModule proc) ++ "_" ++ procName proc
  return $ comment (strSExpr [] proc) #
           (begin $ knowSetForgetAll(pt_knows) #
                    foldr (#) Nop loop1 #
                    foldr (#) Nop loop2 #
                    pt_proc  <--- name #
                    pt_pc   <---- dEntry #
                    pt_status <-- statusCall #
                    Return
           )

compileBranchAction :: Int -> Branch -> CompilingM Instr
compileBranchAction i br@BTau {} = do
  return $ comment (strSExpr [] br) #
           tryResult <-- tryResultEnabled

compileBranchAction i br@BOutput { brChanIndex = c} = do
  expr <- compileExpr (brData br)
  return $ comment (strSExpr [] br) #
           (begin $ var chan ChannelType #
                    chan <-- unboxChannelValue(pt_env c) #
                    ifthenelse (Not (channelAcquireAndRegister(pt, chan, chans, nbChans)))
                    (
                      tryResult <-- tryResultAbort
                    )
                    (
                      var commit CommitType #
                      var tryResult TryResultEnumType #
                      commit <-- tryOutputAction(chan, tryResult) #
                      ifthen (tryResult =:= tryResultEnabled)
                      (
                        expr #
                        (commit_thread_env commit_refval) <-- pt_val #
                        awake(scheduler, commit_thread, commit)
                      )
                    )
           )

compileBranchAction i br@BInput { brChanIndex = c, brBindIndex = x } = do
  return $ comment (strSExpr [] br) #
           (begin $ var chan ChannelType #
                    chan <-- unboxChannelValue(pt_env c) #
                    ifthenelse (Not (channelAcquireAndRegister(pt, chan, chans, nbChans)))
                    (
                      tryResult <-- tryResultAbort
                    )
                    (
                      var commit CommitType #
                      var tryResult TryResultEnumType #
                      commit <-- tryInputAction(chan, tryResult) #
                      ifthen (tryResult =:= tryResultEnabled)
                      (
                        (pt_env x) <-- commit_evalfunc(commit_thread) #
                        (case brBindTyp br of
                           TChannel {} -> ifthen (knowRegister(pt_knows, pt_env x))
                                          (
                                            channelIncrRefCount(pt_env x)
                                          )
                           _           -> Nop
                        ) #
                        awake(scheduler, commit_thread, commit)
                      )
                    )
           )
           
compileAction :: Action -> CompilingM Instr
compileAction act@AOutput { actChanIndex = c } = do
  prefixStart <- genLabel
  prefixCont  <- genLabel
  exprComp    <- compileExpr (actData act)
  evalFunc    <- registerEvalFunc $ exprComp # ReturnVal
  return $ comment (strSExpr [] act) #
           Case prefixStart #
           (begin $ var chan ChannelType #
                    chan <-- unboxChannelValue(pt_env c) #
                    var ok BoolType #
                    ok <-- processAcquireChannel(pt, chan) #
                    ifthen (Not ok)
                    (
                      pt_pc <---- prefixStart #
                      processYield(pt, scheduler) #
                      Return
                    ) #
                    var commit CommitType #
                    var tryResult TryResultEnumType #
                    commit <-- tryOutputAction(chan, tryResult) #
                    ifthenelse (tryResult =:= tryResultEnabled)
                    (
                      releaseChannel(chan) #
                      exprComp #
                      (commit_thread_env commit_refval) <-- pt_val #
                      awake(scheduler, commit_thread, commit) #
                      Goto prefixCont
                    )
                    (
                      ifthen (tryResult =:= tryResultDisabled)
                      (
                        releaseChannel(chan) #
                        processEnd(pt, statusBlocked) #
                        Return
                      )
                    ) #
                    registerOutputCommitment (pt, chan, evalFunc, prefixCont) #
                    acquire(pt_lock) #
                    releaseChannel(chan) #
                    processWait(pt, scheduler) #
                    Return
           ) #
           CaseAndLabel prefixCont

compileAction act@AInput { actChanIndex = c, actBindIndex = x } = do
  prefixStart <- genLabel
  prefixCont  <- genLabel
  return $ comment (strSExpr [] act) #
           Case prefixStart #
           (begin $ var chan ChannelType #
                    chan <-- unboxChannelValue(pt_env c) #
                    var ok BoolType #
                    ok <-- processAcquireChannel(pt, chan) #
                    ifthen (Not ok)
                    (
                      pt_pc <---- prefixStart #
                      processYield(pt, scheduler) #
                      Return
                    ) #
                    var commit CommitType #
                    var tryResult TryResultEnumType #
                    commit <-- tryInputAction(chan, tryResult) #
                    ifthenelse (tryResult =:= tryResultEnabled)
                    (
                      releaseChannel(chan) #
                      commit_evalfunc(commit_thread) #
                      (pt_env x) <-- commit_thread_val #
                      (case actBindTyp act of
                         TChannel {} -> ifthen (knowRegister(pt_knows, pt_env x))
                                        (
                                          channelRef(pt_env x)
                                        )
                         _           -> Nop
                      ) #
                      awake(scheduler, commit_thread, commit) #
                      Goto prefixCont
                    )
                    (
                      ifthen (tryResult =:= tryResultDisabled)
                      (
                        releaseChannel(chan) #
                        processEnd(pt, statusBlocked) #
                        Return
                      )
                    ) #
                    registerInputCommitment(pt, chan, x, prefixCont) #
                    acquire(pt_lock) #
                    releaseChannel(chan) #
                    processWait(pt, scheduler) #
                    Return
           ) #
           CaseAndLabel prefixCont

compileAction act@ANew { actBindIndex = c } = do
  return $ comment (strSExpr [] act) #
           initChannelValue (pt_env c, generateChannel())

compileAction act@ALet { actBindIndex = x } = do
  expr <- compileExpr (actVal act)
  return $ comment (strSExpr [] act) #
           expr #
           (pt_env x) <-- pt_val

compileAction act@ASpawn  {} = do
  CompState { currentModule = currentModName } <- get
  let name | null (actModule act) = (delete '/' $ currentModName) ++ "_" ++ actName act
           | otherwise            = (delete '/' $ actModule act) ++ "_" ++ actName act
  loop <- forM (zip ([1..]::[Int]) (actArgs act)) $ \(i, arg) -> do
    expr <- compileExpr arg
    return $ expr #
             var (v i) PiValueType #
             (v i) <-- pt_val #
             (case exprTyp arg of
                TChannel {} -> knowRegister(child_knows, v i) # incrRefCount(v i)
                _           -> Nop
             ) #
             (child_env (i-1)) <-- (v i)
  return $ comment (strSExpr [] act) #
           (begin $ var child PiThreadType #
                    child <-- generatePiThread(10) # -- TODO: use the real env size !
                    foldr (#) Nop loop #
                    child_proc  <--- name #
                    child_pc   <---- dEntry #
                    child_status <-- statusRun #
                    readyQueuePush(scheduler_ready, child)
           )

compileAction act@APrim {} = do
  loops <- forM (zip ([1..]::[Int]) (actArgs act)) $ \(i, arg) -> do
    expr <- compileExpr arg
    let vi = v i
    return $ (vi, var vi PiValueType #
                  expr #
                  vi <-- pt_val
             )
  let (vs, loop) = unzip loops
  let primName   = PrimName (actModule act) (actName act)
  return $ comment (strSExpr [] act) #
           (begin $ foldr (#) Nop loop #
                    primCall(pt_val, primName, vs)
           )

compileExpr:: Expr -> CompilingM Instr
compileExpr ETrue     {} = return $ initBoolTrue(pt_val)
compileExpr EFalse    {} = return $ initBoolFalse(pt_val)
compileExpr e@EInt    {} = return $ initIntValue(pt_val, exprInt e)
compileExpr e@EString {} = return $ initStringValue(pt_val, exprStr e)
compileExpr e@EVar    {} = return $ pt_val <-- pt_env (exprIndex e)
compileExpr ETuple    {} = throwError $ TodoError "tuples not yet supported"

compileExpr e@EPrim   {} = do
  loops <- forM (zip ([1..]::[Int]) (exprArgs e)) $ \(i, arg) -> do
    expr <- compileExpr arg
    return $ (v i, var (v i) PiValueType #
                   expr #
                   (v i) <-- pt_val
             )
  let (vs, loop) = unzip loops
  let primName   = PrimName (exprModule e) (exprName e)
  return $ begin $ foldr (#) Nop loop #
                   primCall(pt_val, primName, vs)

compileExpr e@EAnd    {} = do
  i1 <- compileExpr (exprLeft e)
  i2 <- compileExpr (exprRight e)
  return $ i1 #
           ifthen (boolFromValue pt_val) i2

compileExpr e@EOr     {} = do
  i1 <- compileExpr (exprLeft e)
  i2 <- compileExpr (exprRight e)
  return $ i1 #
           ifthen (Not (boolFromValue pt_val)) i2

