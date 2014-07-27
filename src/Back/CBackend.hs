{-|
Module         : Back.CBackend
Description    : A C backend module
Stability      : experimental

This backend targets C code.
-}
module Back.CBackend (emitCode) where

import Back.SeqAST
import Back.CodeEmitter

import Data.List (delete)
import Control.Monad

emitCode :: String -> Instr -> EmitterM ()
emitCode mainName instr = do
  emitLn "#include <runtime.h>"
  emitLn "#include <value.h>"
  emitLn "#include <queue.h>"
  emitLn "#include <pi_thread_repr.h>"
  emitLn "#include <knownset_repr.h>"
  emitLn "#include <commit_repr.h>"
  emitLn "#include <scheduler_repr.h>"
  emitLn "#include <try_action.h>"
  emitLn "#include <primitives.h>"
  emitLn ""
  emitDecls instr
  emitLn ""
  emitInstr instr
  emitLn ""
  emitLn "int main() {"
  incrIndent
  emitLn $ "PICC_main(4, " ++ mainName ++ ", 2, 2, 2, 10, 0, 1);" -- TODO : change 10 for the real envsize of entry def
  decrIndent
  emitLn "}"

emitDecls :: Instr -> EmitterM ()
emitDecls Nop = return ()
emitDecls (Seq (DefFunction name ((p1, t1), (p2, t2)) _) i) = do
  emitStr "void "
  emitDefName name
  emitStr "("
  emitType t1
  emitStr " "
  emitVarName p1
  emitStr ", "
  emitType t2
  emitStr " "
  emitVarName p2
  emitStr ");\n"
  emitDecls i
emitDecls (Seq (EvalFunction name (p1, t1) _) i) = do
  emitStr "PICC_Value "
  emitEvalfuncName name
  emitStr "("
  emitType t1
  emitStr " "
  emitVarName p1
  emitStr ");\n"
  emitDecls i
emitDecls _ = error "only declarefun should be in seqast toplevel"

emitInstr :: Instr -> EmitterM ()
emitInstr (Comment str)   = emitLn $ "/* " ++ str ++ " */"
emitInstr (Debug str)     = emitLn $ "printf(\"%s\", \"" ++ str ++ "\");"
emitInstr (Seq i1 i2)     = do
  emitInstr i1
  emitInstr i2
emitInstr (LexBloc i)     = do
  emitLn "{"
  incrIndent
  emitInstr i
  decrIndent
  emitLn "}"
emitInstr (ComBloc loc i) = do
  emitInstr (Comment $ "compilation bloc [" ++ show loc ++ "]")
  emitInstr i
emitInstr Nop = return ()
emitInstr (DefFunction name ((p1, t1), (p2, t2)) body) = do
  emitLn ""
  emitIndent
  emitStr "void "
  emitDefName name
  emitStr "("
  emitType t1
  emitStr " "
  emitVarName p1
  emitStr ", "
  emitType t2
  emitStr " "
  emitVarName p2
  emitStr ") {\n"
  incrIndent
  emitInstr body
  decrIndent
  emitLn "}\n"
emitInstr (EvalFunction name (p1, t1) body) = do
  emitLn ""
  emitIndent
  emitStr "PICC_Value "
  emitEvalfuncName name
  emitStr "("
  emitType t1
  emitStr " "
  emitVarName p1
  emitStr ") {\n"
  incrIndent
  emitInstr body
  decrIndent
  emitLn "}\n"
emitInstr (DeclareVar v t)   = do
  emitIndent
  emitType t
  emitStr " "
  emitVarName v
  emitStr ";\n"
emitInstr (Assign var expr)  = do
  emitIndent
  emitVarName var
  emitStr " = "
  emitBExpr expr
  emitStr ";\n"
emitInstr Return             = emitLn "return;"
emitInstr ReturnVal          = emitLn "return pt->val;"
emitInstr (Goto lbl)         = emitLn $ "goto label" ++ show lbl ++ ";"
emitInstr (Increment var)    = do
  emitIndent
  emitVarName var
  emitStr "++;\n"
emitInstr (Decrement var)    = do
  emitIndent
  emitVarName var
  emitStr "--;\n"
emitInstr (If test cons alt) = do
  emitIndent
  emitStr "if("
  emitBExpr test
  emitStr ") {\n"
  incrIndent
  emitInstr cons
  decrIndent
  emitIndent
  emitStr "}"
  case alt of
    Nop -> return ()
    _   -> do emitStr " else {\n"
              incrIndent
              emitInstr alt
              decrIndent
              emitIndent
              emitStr "}"
  emitStr "\n"
emitInstr (Switch test body) = do
  emitIndent
  emitStr "switch("
  emitBExpr test
  emitStr ") {\n"
  incrIndent
  emitInstr body
  decrIndent
  emitLn "}"
emitInstr (Case lbl)         = do
  decrIndent
  emitLn $ "case " ++ show lbl ++ ":"
  incrIndent
emitInstr (CaseAndLabel lbl) = do
  decrIndent
  emitLn $ "case " ++ show lbl ++ ":"
  emitLn $ "label" ++ show lbl ++ ":"
  incrIndent
emitInstr (ProcCall fun)     = do
  emitIndent
  emitRTFun fun
  emitStr ";\n"

emitBExpr :: BExpr -> EmitterM ()
emitBExpr (Not expr)        = do
  emitStr "!("
  emitBExpr expr
  emitStr ")"
emitBExpr (Equal e1 e2)     = do
  emitBExpr e1
  emitStr " == "
  emitBExpr e2
emitBExpr (IntExpr i)       = emitStr $ show i
emitBExpr (StringExpr str)  = emitStr str
emitBExpr (Var v)           = emitVarName v
emitBExpr (Enum u)          = emitEnumName u
emitBExpr (FunCall fun)     = emitRTFun fun
emitBExpr (FunVal (EvalFunc f)) = emitEvalfuncName f
emitBExpr (FunVal _) = error "FunVal of a function that should not be manipulated as a value"

emitRTFun :: RTFun -> EmitterM ()
emitRTFun (EvalFunc n) = do
  emitEvalfuncName n
  emitStr "(NULL)"
emitRTFun (CommitEvalFunc [pt]) = do
  emitStr "commit->content.out->eval_func("
  emitBExpr pt
  emitStr ")"
emitRTFun (PrimCall n (ptVal : vs)) = do
  emitPrimName n
  emitStr "("
  emitStr "&" ; emitBExpr ptVal
  _ <- forM vs $ \v -> do { emitStr ", " ; emitStr "&" ; emitBExpr v }
  emitStr ")"
emitRTFun (GenerateChannel []) =
  emitStr "PICC_create_channel()"
emitRTFun (GeneratePiThread [envSize]) = do
  emitStr "PICC_create_pithread("
  emitBExpr envSize
  emitStr ", 10, 10)" -- TODO: fix params
emitRTFun (ProcessWait [pt, scheduler]) = do
  emitStr "PICC_process_wait("
  emitBExpr pt
  emitStr ", "
  emitBExpr scheduler
  emitStr ")"
emitRTFun (ProcessEnd [pt, status]) = do
  emitStr "PICC_process_end("
  emitBExpr pt
  emitStr ", " ; emitBExpr status
  emitStr ")"
emitRTFun (ProcessYield [pt, scheduler]) = do
  emitStr "PICC_process_yield("
  emitBExpr pt
  emitStr ", "
  emitBExpr scheduler
  emitStr ")"
emitRTFun (ProcessAcquireChannel [pt, chan]) = do
  emitStr "PICC_process_acquire_channel("
  emitBExpr pt
  emitStr ", "
  emitBExpr chan
  emitStr ")"
emitRTFun (Acquire [ptLock]) = do
  emitStr "PICC_acquire("
  emitBExpr ptLock
  emitStr ")"
emitRTFun (ReleaseChannel [chan]) = do
  emitStr "RELEASE_CHANNEL("
  emitBExpr chan
  emitStr ")"
emitRTFun (ReleaseAllChannels [chans, nbChans]) = do
  emitStr "PICC_release_all_channels("
  emitBExpr chans
  emitStr ", "
  emitBExpr nbChans
  emitStr ")"
emitRTFun (ChannelRef [chan]) = do
  emitStr "PICC_channel_ref("
  emitBExpr chan
  emitStr ")"
emitRTFun (ChannelIncrRefCount [chan]) = do
  emitStr "PICC_handle_incr_ref_count("
  emitStr "PICC_GET_HANDLE("
  emitStr "&" ; emitBExpr chan
  emitStr "))"
emitRTFun (ChannelAcquireAndRegister [pt, chan, chans, nbChans]) = do
  emitStr "PICC_channel_acquire_and_register("
  emitBExpr pt
  emitStr ", "
  emitBExpr chan
  emitStr ", "
  emitBExpr chans
  emitStr ", "
  emitStr "&" ; emitBExpr nbChans
  emitStr ")"
emitRTFun (Awake [sched, commitThread, commit]) = do
  emitStr "PICC_awake("
  emitBExpr sched
  emitStr ", "
  emitBExpr commitThread
  emitStr ", "
  emitBExpr commit
  emitStr ")"
emitRTFun (TryInputAction [commit, tryResult]) = do
  emitStr "PICC_try_input_action("
  emitBExpr commit
  emitStr ", "
  emitStr "&" ; emitBExpr tryResult
  emitStr ")"
emitRTFun (TryOutputAction [commit, tryResult]) = do
  emitStr "PICC_try_output_action("
  emitBExpr commit
  emitStr ", "
  emitStr "&" ; emitBExpr tryResult
  emitStr ")"
emitRTFun (RegisterInputCommitment [pt, chan, x, contLabel]) = do
  emitStr "PICC_register_input_commitment("
  emitBExpr pt
  emitStr ", "
  emitBExpr chan
  emitStr ", "
  emitBExpr x
  emitStr ", "
  emitBExpr contLabel
  emitStr ")"
emitRTFun (RegisterOutputCommitment [pt, chan, efun, contLabel]) = do
  emitStr "PICC_register_output_commitment("
  emitBExpr pt
  emitStr ", "
  emitBExpr chan
  emitStr ", "
  emitBExpr efun
  emitStr ", "
  emitBExpr contLabel
  emitStr ")"
emitRTFun (KnowRegister [ptKnows, chan]) = do
  emitStr "PICC_knownset_register("
  emitBExpr ptKnows
  emitStr ", "
  emitStr "(PICC_KnownValue*)&" ; emitBExpr chan
  emitStr ")"
emitRTFun (KnowSetForgetAll [ptKnows]) = do
  emitStr "PICC_knownset_forget_all("
  emitBExpr ptKnows
  emitStr ")"
emitRTFun (ReadyQueuePush [schedReady, child]) = do
  emitStr "PICC_ready_queue_add("
  emitBExpr schedReady
  emitStr ", "
  emitBExpr child
  emitStr ")"
emitRTFun (InitBoolTrue [ptVal]) = do
  emitStr "PICC_INIT_BOOL_TRUE("
  emitStr "&" ; emitBExpr ptVal
  emitStr ")"
emitRTFun (InitBoolFalse [ptVal]) = do
  emitStr "PICC_INIT_BOOL_FALSE("
  emitStr "&" ; emitBExpr ptVal
  emitStr ")"
emitRTFun (InitIntValue [ptVal, i]) = do
  emitStr "PICC_INIT_INT_VALUE("
  emitStr "&" ; emitBExpr ptVal
  emitStr ", " ; emitBExpr i
  emitStr ")"
emitRTFun (InitStringValue [ptVal, s]) = do
  emitStr "PICC_INIT_STRING_VALUE("
  emitStr "&" ; emitBExpr ptVal
  emitStr ", " ; emitStr "PICC_create_string_handle("
  emitBExpr s
  emitStr "))"
emitRTFun (BoolFromValue [ptVal]) = do
  emitStr "PICC_bool_of_bool_value("
  emitBExpr ptVal
  emitStr ")"
emitRTFun (InitChannelValue [ptVal, chan]) = do
  emitStr "PICC_INIT_CHANNEL_VALUE("
  emitStr "&" ; emitBExpr ptVal
  emitStr ", " ; emitBExpr chan
  emitStr ")"
emitRTFun (UnboxChannelValue [ptVal]) = do
  emitStr "CHANNEL_OF_CHANNEL_VALUE("
  emitStr "&" ; emitBExpr ptVal
  emitStr ")"
emitRTFun (UnboxBoolValue [ptVal]) = do
  emitStr "PICC_BOOL_OF_BOOL_VALUE("
  emitStr "&" ; emitBExpr ptVal
  emitStr ")"
emitRTFun (CreateEmptyChannelArray [n]) = do
  emitStr "PICC_create_empty_channel_array("
  emitBExpr n
  emitStr ")"
emitRTFun _ = error "bad params when applying fun of proc"

emitEnumName :: EnumName -> EmitterM ()
emitEnumName StatusRun         = emitStr "PICC_STATUS_RUN"
emitEnumName StatusBlocked     = emitStr "PICC_STATUS_BLOCKED"
emitEnumName StatusCall        = emitStr "PICC_STATUS_CALL"
emitEnumName StatusEnded       = emitStr "PICC_STATUS_ENDED"
emitEnumName TryResultEnabled  = emitStr "PICC_TRY_ENABLED"
emitEnumName TryResultDisabled = emitStr "PICC_TRY_DISABLED"
emitEnumName TryResultAbort    = emitStr "PICC_TRY_ABORT"

emitType :: Type -> EmitterM ()
emitType BoolType          = emitStr "int"
emitType IntType           = emitStr "int"
emitType PiValueType       = emitStr "PICC_Value"
emitType PiThreadType      = emitStr "PICC_PiThread*"
emitType SchedulerType     = emitStr "PICC_SchedPool*"
emitType ChannelType       = emitStr "PICC_Channel*"
emitType CommitType        = emitStr "PICC_Commit*"
emitType ChannelArrayType  = emitStr "PICC_Channel**"
emitType TryResultEnumType = emitStr "PICC_TryResult"

emitVarName :: VarName -> EmitterM ()
emitVarName PiThread            = emitStr "pt"
emitVarName PiThreadVal         = emitStr "pt->val"
emitVarName PiThreadPC          = emitStr "pt->pc"
emitVarName (PiThreadEnv i)     = emitStr $ "pt->env[" ++ show i ++ "]"
emitVarName PiThreadKnows       = emitStr "pt->knowns"
emitVarName PiThreadLock        = emitStr "pt->lock"
emitVarName PiThreadFuel        = emitStr "pt->fuel"
emitVarName PiThreadProc        = emitStr "pt->proc"
emitVarName PiThreadStatus      = emitStr "pt->status"
emitVarName (PiThreadEnabled i) = emitStr $ "pt->enabled[" ++ show i ++ "]"
emitVarName Child               = emitStr "child"
emitVarName ChildPC             = emitStr "child->pc"
emitVarName ChildStatus         = emitStr "child->status"
emitVarName ChildProc           = emitStr "child->proc"
emitVarName (ChildEnv i)        = emitStr $ "child->env[" ++ show i ++ "]"
emitVarName ChildKnows          = emitStr "child->knowns"
emitVarName Scheduler           = emitStr "scheduler"
emitVarName SchedulerReady      = emitStr "scheduler->ready"
emitVarName (V i)               = emitStr $ "v" ++ show i
emitVarName Chan                = emitStr "chan"
emitVarName Chans               = emitStr "chans"
emitVarName NbChans             = emitStr "nbChans"
emitVarName Commit              = emitStr "commit"
emitVarName CommitThread        = emitStr "commit->thread"
emitVarName CommitThreadVal     = emitStr "commit->thread->val"
emitVarName (CommitThreadEnv v) = do
  emitStr "commit->thread->env["
  emitVarName v
  emitStr "]"
emitVarName CommitVal           = emitStr "commit->val"
emitVarName CommitRefval        = emitStr "commit->content.in->refvar"
emitVarName TryResult           = emitStr "try_result"
emitVarName OK                  = emitStr "ok"
emitVarName NbDisabled          = emitStr "nbDisabled"

emitEvalfuncName :: EvalfuncName -> EmitterM ()
emitEvalfuncName (EvalfuncName i) = emitStr $ "eval" ++ show i

emitPrimName :: PrimName -> EmitterM ()
emitPrimName (PrimName m f) = do
  emitStr $ delete '/' m
  emitStr "_"
  emitStr f

emitDefName :: DefName -> EmitterM ()
emitDefName (DefName m f) = do
  emitStr $ delete '/' m
  emitStr "_"
  emitStr f

