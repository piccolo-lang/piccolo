{-|
Module         : Backend.SeqASTUtils
Description    : SeqAST constructors
Stability      : experimental

This module contains 'Back.SeqAST' utils functions and constructors for
compilation DSL.
-}
module Backend.SeqASTUtils where

import Backend.SeqAST


-- language constructions

var :: VarName -> Type -> Instr
var = DeclareVar

comment :: Show a => a -> Instr
comment a = Comment ("---------- " ++ show a ++ " ----------")

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


-- enumeration values

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


-- primitive call

primCall :: (BExpr, PrimName, [VarName]) -> Instr
primCall (th, pName, vs) = ProcCall (PrimCall pName (th : map Var vs))


-- pithread functions

piThreadCreate :: (Int, Int) -> BExpr
piThreadCreate (envSize, enabledSize) = FunCall $ PiThreadCreate (IntExpr envSize) (IntExpr enabledSize)

setProc :: (BExpr, String, String) -> Instr
setProc (thread, proc1, proc2) = ProcCall $ SetProc thread (FunVal (DefProc (DefName proc1 proc2)))

getPC :: BExpr -> BExpr
getPC thread = FunCall $ GetPC thread

setPC :: (BExpr, Int) -> Instr
setPC (thread, pc) = ProcCall $ SetPC thread (IntExpr pc)

getRegister :: BExpr -> BExpr
getRegister thread = FunCall $ GetRegister thread

setRegister :: (BExpr, BExpr) -> Instr
setRegister (thread, val) = ProcCall $ SetRegister thread val

registerPointer :: BExpr -> BExpr
registerPointer thread = FunCall $ RegisterPointer thread

getEnv :: (BExpr, Int) -> BExpr
getEnv (thread, ind) = FunCall $ GetEnv thread (IntExpr ind)

setEnv :: (BExpr, Int, BExpr) -> Instr
setEnv (thread, ind, val) = ProcCall $ SetEnv thread (IntExpr ind) val

setEnv' :: (BExpr, BExpr, BExpr) -> Instr
setEnv' (thread, ind, val) = ProcCall $ SetEnv thread ind val

getEnabled :: (BExpr, Int) -> BExpr
getEnabled (thread, ind) = FunCall $ GetEnabled thread (IntExpr ind)

setEnabled :: (BExpr, Int, Int) -> Instr
setEnabled (thread, ind, val) = ProcCall $ SetEnabled thread (IntExpr ind) (IntExpr val)

setEnabled' :: (BExpr, Int, BExpr) -> Instr
setEnabled' (thread, ind, val) = ProcCall $ SetEnabled thread (IntExpr ind) val

setStatus :: (BExpr, BExpr) -> Instr
setStatus (thread, status) = ProcCall $ SetStatus thread status

setSafeChoice :: (BExpr, Bool) -> Instr
setSafeChoice (thread, b) = ProcCall $ SetSafeChoice thread (BoolExpr b)

getFuel :: BExpr -> BExpr
getFuel thread = FunCall $ GetFuel thread

decrFuel :: BExpr -> Instr
decrFuel thread = ProcCall $ DecrFuel thread

forgetAllValues :: BExpr -> Instr
forgetAllValues thread = ProcCall $ ForgetAllValues thread

registerEnvValue :: (BExpr, Int) -> Instr
registerEnvValue (thread, ind) = ProcCall $ RegisterEnvValue thread (IntExpr ind)

registerRegisterValue :: BExpr -> Instr
registerRegisterValue thread = ProcCall $ RegisterRegisterValue thread

processLock :: BExpr -> Instr
processLock thread = ProcCall $ ProcessLock thread

processLockChannel :: (BExpr, BExpr) -> BExpr
processLockChannel (thread, channel) = FunCall $ ProcessLockChannel thread channel

processYield :: (BExpr, BExpr) -> Instr
processYield (thread, sched) = ProcCall $ ProcessYield thread sched

processWait :: (BExpr, BExpr) -> Instr
processWait (thread, sched) = ProcCall $ ProcessWait thread sched

processAwake :: (BExpr, BExpr) -> Instr
processAwake (com, sched) = ProcCall $ ProcessAwake com sched

processEnd :: (BExpr, BExpr) -> Instr
processEnd (thread, status) = ProcCall $ ProcessEnd thread status


-- value functions

initNoValue :: BExpr -> Instr
initNoValue valPtr = ProcCall $ InitNoValue valPtr

initBoolTrue :: BExpr -> Instr
initBoolTrue valPtr = ProcCall $ InitBoolTrue valPtr

initBoolFalse :: BExpr -> Instr
initBoolFalse valPtr = ProcCall $ InitBoolFalse valPtr

initIntValue :: (BExpr, Int) -> Instr
initIntValue (valPtr, i) = ProcCall $ InitIntValue valPtr (IntExpr i)

initFloatValue :: (BExpr, Float) -> Instr
initFloatValue (valPtr, f) = ProcCall $ InitFloatValue valPtr (FloatExpr f)

initStringValue :: (BExpr, String) -> Instr
initStringValue (valPtr, s) = ProcCall $ InitStringValue valPtr (StringExpr s)

initChannelValue :: BExpr -> Instr
initChannelValue valPtr = ProcCall $ InitChannelValue valPtr

unboxChannelValue :: BExpr -> BExpr
unboxChannelValue valPtr = FunCall $ UnboxChannelValue valPtr

unboxBoolValue :: BExpr -> BExpr
unboxBoolValue valPtr = FunCall $ UnboxBoolValue valPtr

unlockChannel :: BExpr -> Instr
unlockChannel channel = ProcCall $ UnlockChannel channel


-- scheduler functions

readyPushFront :: (BExpr, BExpr) -> Instr
readyPushFront (rqueue, thread) = ProcCall $ ReadyPushFront rqueue thread

schedGetReadyQueue :: BExpr -> BExpr
schedGetReadyQueue sched = FunCall $ SchedGetReadyQueue sched


-- commitment functions

getThread :: BExpr -> BExpr
getThread commitment = FunCall $ GetThread commitment

getRefVar :: BExpr -> BExpr
getRefVar commitment = FunCall $ GetRefVar commitment

callEvalFunc :: BExpr -> BExpr
callEvalFunc commitment = FunCall $ CallEvalFunc commitment

registerInputCommitment :: (BExpr, BExpr, Int, Int) -> Instr
registerInputCommitment (thread, channel, envval, cont) = ProcCall $
  RegisterInputCommitment thread channel (IntExpr envval) (IntExpr cont)

registerOutputCommitment :: (BExpr, BExpr, EvalfuncName, Int) -> Instr
registerOutputCommitment (thread, channel, f, cont) = ProcCall $
  RegisterOutputCommitment thread channel (FunVal (EvalFunc f)) (IntExpr cont)


-- action functions

tryInputAction :: (BExpr, BExpr) -> BExpr
tryInputAction (channel, res) = FunCall $ TryInputAction channel res

tryOutputAction :: (BExpr, BExpr) -> BExpr
tryOutputAction (channel, res) = FunCall $ TryOutputAction channel res

channelArrayCreate :: Int -> BExpr
channelArrayCreate n = FunCall $ ChannelArrayCreate (IntExpr n)

channelArrayLockAndRegister :: (BExpr, BExpr, BExpr, BExpr) -> BExpr
channelArrayLockAndRegister (channels, nb, thread, channel) =
  FunCall $ ChannelArrayLockAndRegister channels nb thread channel

channelArrayUnlock :: (BExpr, BExpr) -> Instr
channelArrayUnlock (channels, nb) = ProcCall $ ChannelArrayUnlock channels nb


-- vars

class BackendVars a where
  pt                    :: a
  child                 :: a
  scheduler             :: a
  v                     :: Int -> a
  chan                  :: a
  chans                 :: a
  nbChans               :: a
  commit                :: a
  tryResult             :: a
  ok                    :: a
  nbDisabled            :: a

instance BackendVars VarName where
  pt                    = PiThread
  child                 = Child
  scheduler             = Scheduler
  v                     = V
  chan                  = Chan
  chans                 = Chans
  nbChans               = NbChans
  commit                = Commit
  tryResult             = TryResult
  ok                    = Ok
  nbDisabled            = NbDisabled

instance BackendVars BExpr where
  pt                    = Var PiThread
  child                 = Var Child
  scheduler             = Var Scheduler
  v                     = Var . V
  chan                  = Var Chan
  chans                 = Var Chans
  nbChans               = Var NbChans
  commit                = Var Commit
  tryResult             = Var TryResult
  ok                    = Var Ok
  nbDisabled            = Var NbDisabled

