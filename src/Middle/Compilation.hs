{-|
Module         : Middle.Compilation
Description    : Compilation module
Stability      : experimental

This module contains the compilation pass. It transforms a piccolo AST to a sequential AST.
-}
module Middle.Compilation (compilePass) where

import Back.SeqAST
  ( BExpr (Not), DefName (..)
  , EvalfuncName (..), Instr (DefFunction, EvalFunction, Nop
  , Return, ReturnRegister, Goto, Switch, Case, CaseAndLabel)
  , PrimName (..), Type (..)
  )
import Back.SeqASTUtils
import Front.AST
import Front.ASTUtils
import PiccError

import Control.Monad.Error
import Control.Monad.State
import Data.List (delete)

data CompState
  = CompState
    { labelCount    :: Int
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
             Switch (getPC pt)
             (
               Case dEntry #
               procInstrs
             )
           )

compileProcess :: Process -> CompilingM Instr
compileProcess proc@PEnd {} =
  return $ comment proc #
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
             setEnabled' (pt, i, unboxBoolValue (registerPointer pt)) #
             ifthenelse (getEnabled(pt, i))
             (
               act #
               ifthenelse (tryResult =:= tryResultDisabled)
               (
                 setEnabled(pt, i, 0) #
                 incr nbDisabled
               )
               (
                 ifthenelse (tryResult =:= tryResultAbort)
                 (
                   channelArrayUnlock(chans, nbChans) #
                   setPC(pt, startChoice) #
                   processYield(pt, scheduler) #
                   Return
                 )
                 (
                   ifthen (tryResult =:= tryResultEnabled)
                   (
                     channelArrayUnlock(chans, nbChans) #
                     decrFuel pt #
                     ifthen (getFuel pt =:== 0)
                     (
                       setPC(pt, choiceConts !! i) #
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
  loop2 <- forM (zip ([0..]::[Int]) (procBranches proc)) $ \(i, br) -> case br of
    BOutput { brChanIndex = c } -> do
      instrs   <- compileExpr (brData br)
      evalFunc <- registerEvalFunc $ instrs # ReturnRegister
      return $ ifthen (getEnabled(pt, i))
               (
                 registerOutputCommitment(pt, unboxChannelValue(getEnv(pt, c)), evalFunc, choiceConts !! i)
               )
    BInput { brChanIndex = c, brBindIndex = x } ->
      return $ ifthen (getEnabled(pt, i))
               (
                 registerInputCommitment(pt, unboxChannelValue(getEnv(pt, c)), x, choiceConts !! i)
               )
    _ -> return Nop
  loop3 <- forM (zip ([0..]::[Int]) (procBranches proc)) $ \(i, br) -> do
    cont <- compileProcess (brCont br)
    return $ CaseAndLabel (choiceConts !! i) #
             cont
  return $ Case startChoice #
           (begin $ var tryResult TryResultEnumType #
                    var chans ChannelArrayType #
                    chans <-- channelArrayCreate 20 # -- TODO : change 20 for the real number branches
                    var nbDisabled IntType #
                    nbDisabled <---- 0 #
                    var nbChans IntType #
                    nbChans <---- 0 #
                    foldr (#) Nop loop1 #
                    ifthen (nbDisabled =:== n)
                    (
                      channelArrayUnlock(chans, nbChans) #
                      (if (procSafe proc)
                         then setSafeChoice(pt, True)
                         else Nop) #
                      processEnd(pt, statusBlocked) #
                      Return
                    ) #
                    foldr (#) Nop loop2 #
                    processLock pt #
                    channelArrayUnlock(chans, nbChans) #
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
             vi <-- getRegister pt
  loop2 <- forM (zip ([1..]::[Int]) (procArgs proc)) $ \(i, arg) -> do
    let vi = v i
    return $ setEnv(pt, i-1, vi) #
             (if isAManagedType $ exprTyp arg
                then registerEnvValue(pt, i-1)
                else Nop
             )
  CompState { currentModule = currentModName } <- get
  let (name1, name2) | null (procModule proc) = (delete '/' currentModName, procName proc)
                     | otherwise              = (delete '/' $ procModule proc, procName proc)
  return $ comment proc #
           (begin $ forgetAllValues pt #
                    foldr (#) Nop loop1 #
                    foldr (#) Nop loop2 #
                    setProc(pt, name1, name2) #
                    setPC(pt, dEntry) #
                    setStatus(pt, statusCall) #
                    Return
           )

compileBranchAction :: Int -> Branch -> CompilingM Instr
compileBranchAction _ br@BTau {} =
  return $ comment br #
           tryResult <-- tryResultEnabled

compileBranchAction _ br@BOutput { brChanIndex = c} = do
  expr <- compileExpr (brData br)
  return $ comment br #
           (begin $ var chan ChannelType #
                    chan <-- unboxChannelValue(getEnv(pt, c)) #
                    ifthenelse (Not (channelArrayLockAndRegister(chans, nbChans, pt, chan)))
                    (
                      tryResult <-- tryResultAbort
                    )
                    (
                      var commit CommitType #
                      commit <-- tryOutputAction(chan, tryResult) #
                      ifthen (tryResult =:= tryResultEnabled)
                      (
                        expr #
                        setEnv'(getThread commit, getRefVar commit, getRegister pt) #
                        processAwake(commit, scheduler)
                      )
                    )
           )

compileBranchAction _ br@BInput { brChanIndex = c, brBindIndex = x } =
  return $ comment br #
           (begin $ var chan ChannelType #
                    chan <-- unboxChannelValue(getEnv(pt, c)) #
                    ifthenelse (Not (channelArrayLockAndRegister(chans, nbChans, pt, chan)))
                    (
                      tryResult <-- tryResultAbort
                    )
                    (
                      var commit CommitType #
                      commit <-- tryInputAction(chan, tryResult) #
                      ifthen (tryResult =:= tryResultEnabled)
                      (
                        setEnv(pt, x, callEvalFunc commit) #
                        (if isAManagedType $ brBindTyp br
                           then registerEnvValue(pt, x)
                           else Nop
                        ) #
                        processAwake(commit, scheduler)
                      )
                    )
           )

compileAction :: Action -> CompilingM Instr
compileAction act@AOutput { actChanIndex = c } = do
  prefixStart <- genLabel
  prefixCont  <- genLabel
  exprComp    <- compileExpr (actData act)
  evalFunc    <- registerEvalFunc $ exprComp # ReturnRegister
  return $ comment act #
           Case prefixStart #
           (begin $ var chan ChannelType #
                    chan <-- unboxChannelValue(getEnv(pt, c)) #
                    var ok BoolType #
                    ok <-- processLockChannel(pt, chan) #
                    ifthen (Not ok)
                    (
                      setPC(pt, prefixStart) #
                      processYield(pt, scheduler) #
                      Return
                    ) #
                    var commit CommitType #
                    var tryResult TryResultEnumType #
                    commit <-- tryOutputAction(chan, tryResult) #
                    ifthenelse (tryResult =:= tryResultEnabled)
                    (
                      unlockChannel chan #
                      exprComp #
                      setEnv'(getThread commit, getRefVar commit, getRegister pt) #
                      processAwake(commit, scheduler) #
                      Goto prefixCont
                    )
                    (
                      ifthen (tryResult =:= tryResultDisabled)
                      (
                        unlockChannel chan #
                        processEnd(pt, statusBlocked) #
                        Return
                      )
                    ) #
                    registerOutputCommitment (pt, chan, evalFunc, prefixCont) #
                    processLock pt #
                    unlockChannel chan #
                    processWait(pt, scheduler) #
                    Return
           ) #
           CaseAndLabel prefixCont

compileAction act@AInput { actChanIndex = c, actBindIndex = x } = do
  prefixStart <- genLabel
  prefixCont  <- genLabel
  return $ comment act #
           Case prefixStart #
           (begin $ var chan ChannelType #
                    chan <-- unboxChannelValue(getEnv(pt, c)) #
                    var ok BoolType #
                    ok <-- processLockChannel(pt, chan) #
                    ifthen (Not ok)
                    (
                      setPC(pt, prefixStart) #
                      processYield(pt, scheduler) #
                      Return
                    ) #
                    var commit CommitType #
                    var tryResult TryResultEnumType #
                    commit <-- tryInputAction(chan, tryResult) #
                    ifthenelse (tryResult =:= tryResultEnabled)
                    (
                      unlockChannel chan #
                      setEnv(pt, x, callEvalFunc commit) #
                      (if isAManagedType $ actBindTyp act
                         then registerEnvValue(pt, x)
                         else Nop
                      ) #
                      processAwake(commit, scheduler) #
                      Goto prefixCont
                    )
                    (
                      ifthen (tryResult =:= tryResultDisabled)
                      (
                        unlockChannel chan #
                        processEnd(pt, statusBlocked) #
                        Return
                      )
                    ) #
                    registerInputCommitment(pt, chan, x, prefixCont) #
                    processLock pt #
                    unlockChannel chan #
                    processWait(pt, scheduler) #
                    Return
           ) #
           CaseAndLabel prefixCont

compileAction act@ANew { actBindIndex = c } =
  return $ comment act #
           initChannelValue(getEnv(pt, c)) #
           registerEnvValue(pt, c)

compileAction act@ALet { actBindIndex = x } = do
  expr <- compileExpr (actVal act)
  return $ comment act #
           expr #
           setEnv(pt, x, getRegister pt)

compileAction act@ASpawn  {} = do
  CompState { currentModule = currentModName } <- get
  let (name1, name2) | null (actModule act) = (delete '/' currentModName, actName act)
                     | otherwise            = (delete '/' $ actModule act, actName act)
  loop <- forM (zip ([1..]::[Int]) (actArgs act)) $ \(i, arg) -> do
    expr <- compileExpr arg
    return $ expr #
             var (v i) PiValueType #
             (v i) <-- getRegister pt #
             setEnv(child, i-1, v i) #
             (if isAManagedType $ exprTyp arg
                then registerEnvValue(child, i-1)
                else Nop
             )
  return $ comment act #
           (begin $ var child PiThreadType #
                    child <-- piThreadCreate(10, 10) # -- TODO user the computed env sizes !
                    foldr (#) Nop loop #
                    setProc(child, name1, name2) #
                    setPC(child, dEntry) #
                    setStatus(child, statusRun) #
                    readyPushFront(schedGetReadyQueue scheduler, child)
           )

compileAction act@APrim {} = do
  loops <- forM (zip ([1..]::[Int]) (actArgs act)) $ \(i, arg) -> do
    expr <- compileExpr arg
    let vi = v i
    return $ (vi, var vi PiValueType #
                  expr #
                  vi <-- getRegister pt
             )
  let (vs, loop) = unzip loops
  let primName   = PrimName (actModule act) (actName act)
  return $ comment act #
           (begin $ foldr (#) Nop loop #
                    primCall(registerPointer pt, primName, vs)
           )

compileExpr:: Expr -> CompilingM Instr
compileExpr ETrue     {} = return $ initBoolTrue(registerPointer pt)
compileExpr EFalse    {} = return $ initBoolFalse(registerPointer pt)
compileExpr e@EInt    {} = return $ initIntValue(registerPointer pt, exprInt e)
compileExpr e@EString {} = return $ initStringValue(registerPointer pt, exprStr e) # registerRegisterValue pt
compileExpr e@EVar    {} = return $ setRegister(pt, getEnv(pt, exprIndex e))
compileExpr ETuple    {} = error "tuples are not yet implemented"

compileExpr e@EPrim   {} = do
  loops <- forM (zip ([1..]::[Int]) (exprArgs e)) $ \(i, arg) -> do
    expr <- compileExpr arg
    return (v i, var (v i) PiValueType #
                 expr #
                 (v i) <-- getRegister pt
           )
  let (vs, loop) = unzip loops
  let primName   = PrimName (exprModule e) (exprName e)
  return $ begin $ foldr (#) Nop loop #
                   primCall(registerPointer pt, primName, vs)

compileExpr e@EAnd    {} = do
  i1 <- compileExpr (exprLeft e)
  i2 <- compileExpr (exprRight e)
  return $ i1 #
           ifthen (unboxBoolValue (registerPointer pt)) i2

compileExpr e@EOr     {} = do
  i1 <- compileExpr (exprLeft e)
  i2 <- compileExpr (exprRight e)
  return $ i1 #
           ifthen (Not (unboxBoolValue (registerPointer pt))) i2

