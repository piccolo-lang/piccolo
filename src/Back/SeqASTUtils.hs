{-|
Module         : Back.SeqASTUtils
Description    : 
Stability      : experimental

This module contains the generators for constructing a 'SeqAST.Instr' in the compilation pass.
-}
module Back.SeqASTUtils where

import Back.SeqAST


var :: VarName -> Type -> Instr
var = DeclareVar

comment :: String -> Instr
comment str = Comment ("---------- " ++ str ++ " ----------")

(#) :: Instr -> Instr -> Instr
(#) = Seq
infixr 1 #

begin :: Instr -> Instr
begin = LexBloc

(<--) :: VarName -> BExpr -> Instr
(<--) = Assign

(<---) :: VarName -> String -> Instr
(<---) vn s = Assign vn (StringExpr s)

(<----) :: VarName -> Int -> Instr
(<----) vn i = Assign vn (IntExpr i)

incr :: VarName -> Instr
incr = Increment

decr :: VarName -> Instr
decr = Decrement

ifthen :: BExpr -> Instr -> Instr
ifthen e t = If e t Nop

ifthenelse :: BExpr -> Instr -> Instr -> Instr
ifthenelse = If

(=:=) :: BExpr -> BExpr -> BExpr
(=:=) = Equal

(=:==) :: BExpr -> Int -> BExpr
(=:==) e i = Equal e (IntExpr i)

statusRun :: BExpr
statusRun = Enum StatusRun

statusBlocked :: BExpr
statusBlocked = Enum StatusBlocked

statusCall :: BExpr
statusCall = Enum StatusCall

statusEnded :: BExpr
statusEnded = Enum StatusEnded

tryResultEnabled :: BExpr
tryResultEnabled = Enum TryResultEnabled

tryResultDisabled :: BExpr
tryResultDisabled = Enum TryResultDisabled

tryResultAbort :: BExpr
tryResultAbort = Enum TryResultAbort

primCall :: (BExpr, PrimName, [VarName]) -> Instr
primCall (th, pName, vs) = ProcCall (PrimCall pName (th : map Var vs))

initBoolTrue :: BExpr -> Instr
initBoolTrue e1 = ProcCall $ InitBoolTrue [e1]

initBoolFalse :: BExpr -> Instr
initBoolFalse e1 = ProcCall $ InitBoolFalse [e1]

initIntValue :: (BExpr, Int) -> Instr
initIntValue (e1, i2) = ProcCall $ InitIntValue [e1, IntExpr i2]

initStringValue :: (BExpr, String) -> Instr
initStringValue (e1, s2) = ProcCall $ InitStringValue [e1, StringExpr s2]

initChannelValue :: (BExpr, BExpr) -> Instr
initChannelValue (e1, e2) = ProcCall $ InitChannelValue [e1, e2]

unboxChannelValue :: BExpr -> BExpr
unboxChannelValue e1 = FunCall $ UnboxChannelValue [e1]


class BackendVars a where
  pt                    :: a
  pt_val                :: a
  pt_pc                 :: a
  pt_env                :: Int -> a
  pt_knows              :: a
  pt_lock               :: a
  pt_fuel               :: a
  pt_proc               :: a
  pt_status             :: a
  pt_enabled            :: Int -> a
  child                 :: a
  child_pc              :: a
  child_status          :: a
  child_proc            :: a
  child_env             :: Int -> a
  child_knows           :: a
  scheduler             :: a
  scheduler_ready       :: a
  v                     :: Int -> a
  chan                  :: a
  chans                 :: a
  nbChans               :: a
  commit                :: a
  commit_thread         :: a
  commit_thread_val     :: a
  commit_thread_env     :: VarName -> a
  commit_val            :: a
  commit_refval         :: a
  tryResult             :: a
  ok                    :: a
  nbDisabled            :: a

instance BackendVars VarName where
  pt                    = PiThread
  pt_val                = PiThreadVal
  pt_pc                 = PiThreadPC
  pt_env                = PiThreadEnv
  pt_knows              = PiThreadKnows
  pt_lock               = PiThreadLock
  pt_fuel               = PiThreadFuel
  pt_proc               = PiThreadProc
  pt_status             = PiThreadStatus
  pt_enabled            = PiThreadEnabled
  child                 = Child
  child_pc              = ChildPC
  child_status          = ChildStatus
  child_proc            = ChildProc
  child_env             = ChildEnv
  child_knows           = ChildKnows
  scheduler             = Scheduler
  scheduler_ready       = SchedulerReady
  v                     = V
  chan                  = Chan
  chans                 = Chans
  nbChans               = NbChans
  commit                = Commit
  commit_thread         = CommitThread
  commit_thread_val     = CommitThreadVal
  commit_thread_env     = CommitThreadEnv
  commit_val            = CommitVal
  commit_refval         = CommitRefval
  tryResult             = TryResult
  ok                    = OK
  nbDisabled            = NbDisabled

instance BackendVars BExpr where
  pt                    = Var PiThread
  pt_val                = Var PiThreadVal
  pt_pc                 = Var PiThreadPC
  pt_env                = Var . PiThreadEnv
  pt_knows              = Var PiThreadKnows
  pt_lock               = Var PiThreadLock
  pt_fuel               = Var PiThreadFuel
  pt_proc               = Var PiThreadProc
  pt_status             = Var PiThreadStatus
  pt_enabled            = Var . PiThreadEnabled
  child                 = Var Child
  child_pc              = Var ChildPC
  child_status          = Var ChildStatus
  child_proc            = Var ChildProc
  child_env             = Var . ChildEnv
  child_knows           = Var ChildKnows
  scheduler             = Var Scheduler
  scheduler_ready       = Var SchedulerReady
  v                     = Var . V
  chan                  = Var Chan
  chans                 = Var Chans
  nbChans               = Var NbChans
  commit                = Var Commit
  commit_thread         = Var CommitThread
  commit_thread_val     = Var CommitThreadVal
  commit_thread_env     = Var . CommitThreadEnv
  commit_val            = Var CommitVal
  commit_refval         = Var CommitRefval
  tryResult             = Var TryResult
  ok                    = Var OK
  nbDisabled            = Var NbDisabled


class BackendFuns a where
  commit_evalfunc            :: BExpr -> a
  generateChannel            :: () -> a
  generatePiThread           :: Int -> a
  processWait                :: (BExpr, BExpr) -> a
  processEnd                 :: (BExpr, BExpr) -> a
  processYield               :: (BExpr, BExpr) -> a
  processAcquireChannel      :: (BExpr, BExpr) -> a
  acquire                    :: BExpr -> a
  releaseChannel             :: BExpr -> a
  releaseAllChannels         :: (BExpr, BExpr) -> a
  channelRef                 :: BExpr -> a
  channelIncrRefCount        :: BExpr -> a
  channelAcquireAndRegister  :: (BExpr, BExpr, BExpr, BExpr) -> a
  awake                      :: (BExpr, BExpr, BExpr) -> a
  tryInputAction             :: (BExpr, BExpr) -> a
  tryOutputAction            :: (BExpr, BExpr) -> a
  registerInputCommitment    :: (BExpr, BExpr, Int, Int) -> a
  registerOutputCommitment   :: (BExpr, BExpr, EvalfuncName, Int) -> a
  knowRegister               :: (BExpr, BExpr) -> a
  knowSetForgetAll           :: BExpr -> a
  readyQueuePush             :: (BExpr, BExpr) -> a
  boolFromValue              :: BExpr -> a

instance BackendFuns BExpr where
  commit_evalfunc e1         = FunCall $ CommitEvalFunc [e1]
  generateChannel ()         = FunCall $ GenerateChannel []
  generatePiThread i1        = FunCall $ GeneratePiThread [IntExpr i1]
  processWait (e1, e2)       = FunCall $ ProcessWait [e1, e2]
  processEnd (e1, e2)        = FunCall $ ProcessEnd [e1, e2]
  processYield (e1, e2)      = FunCall $ ProcessYield [e1, e2]
  processAcquireChannel (e1, e2) =
    FunCall $ ProcessAcquireChannel [e1, e2]
  acquire e1                 = FunCall $ Acquire [e1]
  releaseChannel e1          = FunCall $ ReleaseChannel [e1]
  releaseAllChannels (e1, e2) =
    FunCall $ ReleaseAllChannels [e1, e2]
  channelRef e1              = FunCall $ ChannelRef [e1]
  channelIncrRefCount e1     = FunCall $ ChannelIncrRefCount [e1]
  channelAcquireAndRegister (e1, e2, e3, e4) =
    FunCall $ ChannelAcquireAndRegister [e1, e2, e3, e4]
  awake (e1, e2, e3)         = FunCall $ Awake [e1, e2, e3]
  tryInputAction (e1, e2)    = FunCall $ TryInputAction [e1, e2]
  tryOutputAction (e1, e2)   = FunCall $ TryOutputAction [e1, e2]
  registerInputCommitment (e1, e2, i3, i4) =
    FunCall $ RegisterInputCommitment [e1, e2, IntExpr i3, IntExpr i4]
  registerOutputCommitment (e1, e2, f3, i4) =
    FunCall $ RegisterOutputCommitment [e1, e2, FunVal (EvalFunc f3), IntExpr i4]
  knowRegister (e1, e2)      = FunCall $ KnowRegister [e1, e2]
  knowSetForgetAll e1        = FunCall $ KnowSetForgetAll [e1]
  readyQueuePush (e1, e2)    = FunCall $ ReadyQueuePush [e1, e2]
  boolFromValue e1           = FunCall $ BoolFromValue [e1]

instance BackendFuns Instr where
  commit_evalfunc e1         = ProcCall $ CommitEvalFunc [e1]
  generateChannel ()         = ProcCall $ GenerateChannel []
  generatePiThread i1        = ProcCall $ GeneratePiThread [IntExpr i1]
  processWait (e1, e2)       = ProcCall $ ProcessWait [e1, e2]
  processEnd (e1, e2)        = ProcCall $ ProcessEnd [e1, e2]
  processYield (e1, e2)      = ProcCall $ ProcessYield [e1, e2]
  processAcquireChannel (e1, e2) =
    ProcCall $ ProcessAcquireChannel [e1, e2]
  acquire e1                 = ProcCall $ Acquire [e1]
  releaseChannel e1          = ProcCall $ ReleaseChannel [e1]
  releaseAllChannels (e1, e2) =
    ProcCall $ ReleaseAllChannels [e1, e2]
  channelRef e1              = ProcCall $ ChannelRef [e1]
  channelIncrRefCount e1     = ProcCall $ ChannelIncrRefCount [e1]
  channelAcquireAndRegister (e1, e2, e3, e4) =
    ProcCall $ ChannelAcquireAndRegister [e1, e2, e3, e4]
  awake (e1, e2, e3)         = ProcCall $ Awake [e1, e2, e3]
  tryInputAction (e1, e2)    = ProcCall $ TryInputAction [e1, e2]
  tryOutputAction (e1, e2)   = ProcCall $ TryOutputAction [e1, e2]
  registerInputCommitment (e1, e2, i3, i4) =
    ProcCall $ RegisterInputCommitment [e1, e2, IntExpr i3, IntExpr i4]
  registerOutputCommitment (e1, e2, f3, i4) =
    ProcCall $ RegisterOutputCommitment [e1, e2, FunVal (EvalFunc f3), IntExpr i4]
  knowRegister (e1, e2)      = ProcCall $ KnowRegister [e1, e2]
  knowSetForgetAll e1        = ProcCall $ KnowSetForgetAll [e1]
  readyQueuePush (e1, e2)    = ProcCall $ ReadyQueuePush [e1, e2]
  boolFromValue e1           = ProcCall $ BoolFromValue [e1]

