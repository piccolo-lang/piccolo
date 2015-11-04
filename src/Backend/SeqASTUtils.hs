{-|
Module         : Backend.SeqASTUtils
Description    : SeqAST constructors
Stability      : experimental

This module contains 'Back.SeqAST' utils functions and constructors for
compilation DSL.
-}
module Backend.SeqASTUtils
  ( -- * Sequential language constructions
    var
  , (#)
  , comment
  , debugEvent
  , begin
  , (<--)
  , (<---)
  , (<----)
  , incr
  , decr
  , ifthen
  , ifthenelse
  , (=:=)
  , (=:==)
    -- * Enumeration values
  , statusRun
  , statusBlocked
  , statusCall
  , statusEnded
  , tryResultEnabled
  , tryResultDisabled
  , tryResultAbort
    -- * Primitives
  , primCall
    -- * Pithread functions
  , piThreadCreate
  , setProc
  , getPC
  , setPC
  , getRegister
  , setRegister
  , registerPointer
  , getEnv
  , setEnv
  , setEnv'
  , getEnabled
  , setEnabled
  , setEnabled'
  , setStatus
  , setSafeChoice
  , getFuel
  , decrFuel
    -- * Values functions
  , forgetAllValues
  , registerEnvValue
  , registerEnvValue'
  , processLock
  , processLockChannel
  , processYield
  , processWait
  , processAwake
  , processEnd
  , initNoValue
  , initBoolTrue
  , initBoolFalse
  , initIntValue
  , initFloatValue
  , initStringValue
  , initChannelValue
  , unboxChannelValue
  , unboxBoolValue
  , unlockChannel
    -- * Scheduler functions
  , readyPushFront
  , schedGetReadyQueue
    -- * Commitments functions
  , getThread
  , getRefVar
  , callEvalFunc
    -- * Actions functions
  , registerInputCommitment
  , registerOutputCommitment
  , tryInputAction
  , tryOutputAction
  , channelArrayCreate
  , channelArrayLockAndRegister
  , channelArrayUnlock
    -- * Backend variables
  , BackendVars (..)
  )
where

import Core.AST
import Backend.SeqAST


-- | Variable declaration
var :: VarName -> Type -> Instr
var = DeclareVar

-- | Comment insertion
comment :: Show a => a -> Instr
comment a = Comment ("---------- " ++ show a ++ " ----------")

-- | Debug event insertion
debugEvent :: (Int, BExpr, BExpr) -> Instr
debugEvent (evtId, pt, sched) =
  PreprocDebugMode $ ProcCall (DebugEvent (IntExpr evtId) pt sched)

-- | Instructions sequence
(#) :: Instr -> Instr -> Instr
(#) = Seq
infixr 1 #

-- | Lexical bloc
begin :: Instr -> Instr
begin = LexBloc

-- | Variable assignment (expression)
(<--) :: VarName -> BExpr -> Instr
(<--) = Assign

-- | Variable assignment (litteral string)
(<---) :: VarName -> String -> Instr
(<---) vn s = Assign vn (StringExpr s)

-- | Variable assignment (litteral integer)
(<----) :: VarName -> Int -> Instr
(<----) vn i = Assign vn (IntExpr i)

-- | Variable incrementation
incr :: VarName -> Instr
incr = Increment

-- | Variable decrementation
decr :: VarName -> Instr
decr = Decrement

-- | Alternative constructor
ifthen :: BExpr -> Instr -> Instr
ifthen e t = If e t Nop

-- | Alternative constructor
ifthenelse :: BExpr -> Instr -> Instr -> Instr
ifthenelse = If

-- | Expression comparison
(=:=) :: BExpr -> BExpr -> BExpr
(=:=) = Equal

-- | Comparison between an expression (left) and a litteral integer (right)
(=:==) :: BExpr -> Int -> BExpr
(=:==) e i = Equal e (IntExpr i)


-- | Flag for running process
statusRun :: BExpr
statusRun = Enum StatusRun

-- | Flag for blocked process
statusBlocked :: BExpr
statusBlocked = Enum StatusBlocked

-- | Flag for a def-calling process
statusCall :: BExpr
statusCall = Enum StatusCall

-- | Flag for a terminated process
statusEnded :: BExpr
statusEnded = Enum StatusEnded

-- | Flag for a ready try-action
tryResultEnabled :: BExpr
tryResultEnabled = Enum TryResultEnabled

-- | Flag for a deadlocked try-action
tryResultDisabled :: BExpr
tryResultDisabled = Enum TryResultDisabled

-- | Flag for a aborted try-action
tryResultAbort :: BExpr
tryResultAbort = Enum TryResultAbort

-- | Primitive call
primCall :: (BExpr, PrimName, [VarName]) -> Instr
primCall (th, pName, vs) = ProcCall (PrimCall pName (th : map Var vs))

-- | Creating a new process
piThreadCreate :: (Int, Int) -> BExpr
piThreadCreate (envSize, enabledSize) = FunCall $ PiThreadCreate (IntExpr envSize) (IntExpr enabledSize)

-- | Getting the def executed by a process
setProc :: (BExpr, String, String) -> Instr
setProc (thread, proc1, proc2) = ProcCall $ SetProc thread (FunVal (DefProc (DefName proc1 proc2)))

-- | Getting the program counter of a process
getPC :: BExpr -> BExpr
getPC thread = FunCall $ GetPC thread

-- | Setting the program counter of a process
setPC :: (BExpr, Int) -> Instr
setPC (thread, pc) = ProcCall $ SetPC thread (IntExpr pc)

-- | Getting the register of a process by value
getRegister :: BExpr -> BExpr
getRegister thread = FunCall $ GetRegister thread

-- | Setting the register of a process
setRegister :: (BExpr, BExpr) -> Instr
setRegister (thread, val) = ProcCall $ SetRegister thread val

-- | Getting the register of a process by reference
registerPointer :: BExpr -> BExpr
registerPointer thread = FunCall $ RegisterPointer thread

-- | Getting an environment value
getEnv :: (BExpr, Int) -> BExpr
getEnv (thread, ind) = FunCall $ GetEnv thread (IntExpr ind)

-- | Setting an environment value
setEnv :: (BExpr, Int, BExpr) -> Instr
setEnv (thread, ind, val) = ProcCall $ SetEnv thread (IntExpr ind) val

-- | Setting an environment value with index being a runtime expression
setEnv' :: (BExpr, BExpr, BExpr) -> Instr
setEnv' (thread, ind, val) = ProcCall $ SetEnv thread ind val

-- | Getting the status of a choice branch
getEnabled :: (BExpr, Int) -> BExpr
getEnabled (thread, ind) = FunCall $ GetEnabled thread (IntExpr ind)

-- | Setting the status of a choice branch
setEnabled :: (BExpr, Int, Int) -> Instr
setEnabled (thread, ind, val) = ProcCall $ SetEnabled thread (IntExpr ind) (IntExpr val)

-- | Setting the status of a choice branch with index being a runtime expression
setEnabled' :: (BExpr, Int, BExpr) -> Instr
setEnabled' (thread, ind, val) = ProcCall $ SetEnabled thread (IntExpr ind) val

-- | Setting the status of a process
setStatus :: (BExpr, BExpr) -> Instr
setStatus (thread, status) = ProcCall $ SetStatus thread status

-- | Setting safe_choice parameter of a process
setSafeChoice :: (BExpr, Bool) -> Instr
setSafeChoice (thread, b) = ProcCall $ SetSafeChoice thread (BoolExpr b)

-- | Getting the current fuel of a process
getFuel :: BExpr -> BExpr
getFuel thread = FunCall $ GetFuel thread

-- | Consuming the fuel of a process
decrFuel :: BExpr -> Instr
decrFuel thread = ProcCall $ DecrFuel thread

-- | Forgetting all managed values of a process
forgetAllValues :: BExpr -> Instr
forgetAllValues thread = ProcCall $ ForgetAllValues thread

-- | Registering a value as managed by a process
registerEnvValue :: (BExpr, Int) -> Instr
registerEnvValue (thread, ind) = ProcCall $ RegisterEnvValue thread (IntExpr ind)

-- | Registering a value as managed by a process
registerEnvValue' :: (BExpr, BExpr) -> Instr
registerEnvValue' (thread, ind) = ProcCall $ RegisterEnvValue thread ind

-- | Getting the lock on a process
processLock :: BExpr -> Instr
processLock thread = ProcCall $ ProcessLock thread

-- | Getting the lock on a channel
processLockChannel :: (BExpr, BExpr) -> BExpr
processLockChannel (thread, channel) = FunCall $ ProcessLockChannel thread channel

-- | Yielding a process
processYield :: (BExpr, BExpr) -> Instr
processYield (thread, sched) = ProcCall $ ProcessYield thread sched

-- | Placing a process in the wait queue
processWait :: (BExpr, BExpr) -> Instr
processWait (thread, sched) = ProcCall $ ProcessWait thread sched

-- | Awaking a process
processAwake :: (BExpr, BExpr) -> Instr
processAwake (com, sched) = ProcCall $ ProcessAwake com sched

-- | Process termination
processEnd :: (BExpr, BExpr) -> Instr
processEnd (thread, status) = ProcCall $ ProcessEnd thread status

-- | Initialization of a novalue
initNoValue :: BExpr -> Instr
initNoValue valPtr = ProcCall $ InitNoValue valPtr

-- | Initialization of a bool value (true)
initBoolTrue :: BExpr -> Instr
initBoolTrue valPtr = ProcCall $ InitBoolTrue valPtr

-- | Initialization of a bool value (false)
initBoolFalse :: BExpr -> Instr
initBoolFalse valPtr = ProcCall $ InitBoolFalse valPtr

-- | Initialization of an integer value
initIntValue :: (BExpr, Int) -> Instr
initIntValue (valPtr, i) = ProcCall $ InitIntValue valPtr (IntExpr i)

-- | Initialization of a float value
initFloatValue :: (BExpr, Float) -> Instr
initFloatValue (valPtr, f) = ProcCall $ InitFloatValue valPtr (FloatExpr f)

-- | Initialization of a string value
initStringValue :: (BExpr, String) -> Instr
initStringValue (valPtr, s) = ProcCall $ InitStringValue valPtr (StringExpr s)

-- | Initialization of a channel value
initChannelValue :: BExpr -> Instr
initChannelValue valPtr = ProcCall $ InitChannelValue valPtr

-- | Unboxing a channel
unboxChannelValue :: BExpr -> BExpr
unboxChannelValue valPtr = FunCall $ UnboxChannelValue valPtr

-- | Unboxing a boolean
unboxBoolValue :: BExpr -> BExpr
unboxBoolValue valPtr = FunCall $ UnboxBoolValue valPtr

-- | Releasing the lock of a channel
unlockChannel :: BExpr -> Instr
unlockChannel channel = ProcCall $ UnlockChannel channel

-- | Pushing a thread on the ready queue
readyPushFront :: (BExpr, BExpr) -> Instr
readyPushFront (rqueue, thread) = ProcCall $ ReadyPushFront rqueue thread

-- | Getting the readyqueue of the scheduler
schedGetReadyQueue :: BExpr -> BExpr
schedGetReadyQueue sched = FunCall $ SchedGetReadyQueue sched

-- | Getting the thread of a commitment
getThread :: BExpr -> BExpr
getThread commitment = FunCall $ GetThread commitment

-- | Getting the refvar of a commitment
getRefVar :: BExpr -> BExpr
getRefVar commitment = FunCall $ GetRefVar commitment

-- | Calling the evaluation function for the value to output
callEvalFunc :: BExpr -> BExpr
callEvalFunc commitment = FunCall $ CallEvalFunc commitment

-- | Registering an input commitment
registerInputCommitment :: (BExpr, BExpr, Int, Int) -> Instr
registerInputCommitment (thread, channel, envval, cont) = ProcCall $
  RegisterInputCommitment thread channel (IntExpr envval) (IntExpr cont)

-- | Registering an output commitment
registerOutputCommitment :: (BExpr, BExpr, EvalfuncName, Int) -> Instr
registerOutputCommitment (thread, channel, f, cont) = ProcCall $
  RegisterOutputCommitment thread channel (FunVal (EvalFunc f)) (IntExpr cont)

-- | Trying an input
tryInputAction :: (BExpr, BExpr) -> BExpr
tryInputAction (channel, res) = FunCall $ TryInputAction channel res

-- | Trying an output
tryOutputAction :: (BExpr, BExpr) -> BExpr
tryOutputAction (channel, res) = FunCall $ TryOutputAction channel res

-- | Channel array allocation
channelArrayCreate :: Int -> BExpr
channelArrayCreate n = FunCall $ ChannelArrayCreate (IntExpr n)

-- | Channel array lock
channelArrayLockAndRegister :: (BExpr, BExpr, BExpr, BExpr) -> BExpr
channelArrayLockAndRegister (channels, nb, thread, channel) =
  FunCall $ ChannelArrayLockAndRegister channels nb thread channel

-- | Channel array unlock and free
channelArrayUnlock :: (BExpr, BExpr) -> Instr
channelArrayUnlock (channels, nb) = ProcCall $ ChannelArrayUnlock channels nb


-- | Variables are declared in a typeclass so that they can be used either
-- as an expression or as a variable name for function application
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

