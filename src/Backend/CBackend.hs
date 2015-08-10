{-|
Module         : Backend.CBackend
Description    : A C backend module
Stability      : experimental

This backend targets C code.
__TODO__: put the real values of env size and choice size in emitCode
-}
module Backend.CBackend
  ( emitCode
  )
where

import Backend.Codegen
import Backend.SeqAST

import Control.Monad
import Data.List (delete)

-- | Code emitter function for C backend
emitCode :: String -> Instr -> EmitterM ()
emitCode mainName instr = do
  emitLn "#include <runtime.h>"
  emitLn ""
  emitDecls instr
  emitLn ""
  emitInstr instr
  emitLn ""
  emitLn "int main() {"
  incrIndent
  emitLn $ "PICC_main(4, " ++ mainName ++ ", 2, 2, 2, 10, 10);"
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
  emitLn "}"
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
emitInstr ReturnRegister     = emitLn "return PICC_get_register(pt);"
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
  emitStr "switch ("
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
emitBExpr (BoolExpr b)
  | b         = emitStr "1"
  | otherwise = emitStr "0"
emitBExpr (IntExpr i)       = emitStr $ show i
emitBExpr (FloatExpr f)     = emitStr $ show f
emitBExpr (StringExpr str)  = emitStr $ show str
emitBExpr (Var v)           = emitVarName v
emitBExpr (Enum u)          = emitEnumName u
emitBExpr (FunCall fun)     = emitRTFun fun
emitBExpr (FunVal (EvalFunc f)) = emitEvalfuncName f
emitBExpr (FunVal (DefProc f)) = emitDefName f
emitBExpr (FunVal _) = error "bad FunVal"

emitRTFun :: RTFun -> EmitterM ()
emitRTFun (EvalFunc n) = do
  emitEvalfuncName n
  emitStr "(NULL)"
emitRTFun (PrimCall n (valPtr : vs)) = do
  emitPrimName n
  emitStr "("
  emitBExpr valPtr
  forM_ vs $ \v -> do { emitStr ", " ; emitStr "&" ; emitBExpr v }
  emitStr ")"
emitRTFun (PrimCall _ []) = emitStr "PRIMCALL ERROR"
emitRTFun (DefProc (DefName name _)) = emitStr name
emitRTFun (PiThreadCreate envSize enabledSize) = do
  emitStr "PICC_pithread_alloc("
  emitBExpr envSize
  emitStr ", "
  emitBExpr enabledSize
  emitStr ")"
emitRTFun (SetProc pt proc) = do
  emitStr "PICC_set_proc("
  emitBExpr pt
  emitStr ", "
  emitBExpr proc
  emitStr ")"
emitRTFun (GetPC pt) = do
  emitStr "PICC_get_pc("
  emitBExpr pt
  emitStr ")"
emitRTFun (SetPC pt pc) = do
  emitStr "PICC_set_pc("
  emitBExpr pt
  emitStr ", "
  emitBExpr pc
  emitStr ")"
emitRTFun (GetRegister pt) = do
  emitStr "PICC_get_register("
  emitBExpr pt
  emitStr ")"
emitRTFun (SetRegister pt val) = do
  emitStr "PICC_set_register("
  emitBExpr pt
  emitStr ", "
  emitBExpr val
  emitStr ")"
emitRTFun (RegisterPointer pt) = do
  emitStr "PICC_get_register_ptr("
  emitBExpr pt
  emitStr ")"
emitRTFun (GetEnv pt i) = do
  emitStr "PICC_get_env("
  emitBExpr pt
  emitStr ", "
  emitBExpr i
  emitStr ")"
emitRTFun (SetEnv pt i val) = do
  emitStr "PICC_set_env("
  emitBExpr pt
  emitStr ", "
  emitBExpr i
  emitStr ", "
  emitBExpr val
  emitStr ")"
emitRTFun (GetEnabled pt i) = do
  emitStr "PICC_get_enabled("
  emitBExpr pt
  emitStr ", "
  emitBExpr i
  emitStr ")"
emitRTFun (SetEnabled pt i val) = do
  emitStr "PICC_set_enabled("
  emitBExpr pt
  emitStr ", "
  emitBExpr i
  emitStr ", "
  emitBExpr val
  emitStr ")"
emitRTFun (SetStatus pt status) = do
  emitStr "PICC_set_status("
  emitBExpr pt
  emitStr ", "
  emitBExpr status
  emitStr ")"
emitRTFun (SetSafeChoice pt b) = do
  emitStr "PICC_set_safechoice("
  emitBExpr pt
  emitStr ", "
  emitBExpr b
  emitStr ")"
emitRTFun (GetFuel pt) = do
  emitStr "PICC_get_fuel("
  emitBExpr pt
  emitStr ")"
emitRTFun (DecrFuel pt) = do
  emitStr "PICC_decr_fuel("
  emitBExpr pt
  emitStr ")"
emitRTFun (ForgetAllValues pt) = do
  emitStr "PICC_forget_all_values("
  emitBExpr pt
  emitStr ")"
emitRTFun (RegisterEnvValue pt i) = do
  emitStr "PICC_register_env_value("
  emitBExpr pt
  emitStr ", "
  emitBExpr i
  emitStr ")"
emitRTFun (ProcessLock pt) = do
  emitStr "PICC_process_lock("
  emitBExpr pt
  emitStr ")"
emitRTFun (ProcessLockChannel pt chan) = do
  emitStr "PICC_process_lock_channel("
  emitBExpr pt
  emitStr ", "
  emitBExpr chan
  emitStr ")"
emitRTFun (ProcessYield pt sched) = do
  emitStr "PICC_process_yield("
  emitBExpr pt
  emitStr ", "
  emitBExpr sched
  emitStr ")"
emitRTFun (ProcessWait pt sched) = do
  emitStr "PICC_process_wait("
  emitBExpr pt
  emitStr ", "
  emitBExpr sched
  emitStr ")"
emitRTFun (ProcessAwake commit sched) = do
  emitStr "PICC_process_awake("
  emitBExpr commit
  emitStr ", "
  emitBExpr sched
  emitStr ")"
emitRTFun (ProcessEnd pt status) = do
  emitStr "PICC_process_end("
  emitBExpr pt
  emitStr ", "
  emitBExpr status
  emitStr ")"
emitRTFun (InitNoValue valPtr) = do
  emitStr "PICC_novalue_init("
  emitBExpr valPtr
  emitStr ")"
emitRTFun (InitBoolTrue valPtr) = do
  emitStr "PICC_boolvalue_init("
  emitBExpr valPtr
  emitStr ", 1)"
emitRTFun (InitBoolFalse valPtr) = do
  emitStr "PICC_boolvalue_init("
  emitBExpr valPtr
  emitStr ", 0)"
emitRTFun (InitIntValue valPtr i) = do
  emitStr "PICC_intvalue_init("
  emitBExpr valPtr
  emitStr ", "
  emitBExpr i
  emitStr ")"
emitRTFun (InitFloatValue valPtr f) = do
  emitStr "PICC_floatvalue_init("
  emitBExpr valPtr
  emitStr ", "
  emitBExpr f
  emitStr ")"
emitRTFun (InitStringValue valPtr s) = do
  emitStr "PICC_stringvalue_init("
  emitBExpr valPtr
  emitStr ", "
  emitBExpr s
  emitStr ")"
emitRTFun (InitChannelValue valPtr) = do
  emitStr "PICC_channelvalue_init("
  emitBExpr valPtr
  emitStr ")"
emitRTFun (UnboxChannelValue valPtr) = do
  emitStr "PICC_channelvalue_unbox("
  emitBExpr valPtr
  emitStr ")"
emitRTFun (UnboxBoolValue valPtr) = do
  emitStr "PICC_boolvalue_unbox("
  emitBExpr valPtr
  emitStr ")"
emitRTFun (UnlockChannel chan) = do
  emitStr "PICC_unlock_channel("
  emitBExpr chan
  emitStr ")"
emitRTFun (ReadyPushFront rq pt) = do
  emitStr "PICC_readyqueue_push_front("
  emitBExpr rq
  emitStr ", "
  emitBExpr pt
  emitStr ")"
emitRTFun (SchedGetReadyQueue sched) = do
  emitStr "PICC_sched_get_readyqueue("
  emitBExpr sched
  emitStr ")"
emitRTFun (GetThread commit) = do
  emitStr "PICC_commit_get_pithread("
  emitBExpr commit
  emitStr ")"
emitRTFun (GetRefVar commit) = do
  emitStr "PICC_commit_get_refvar("
  emitBExpr commit
  emitStr ")"
emitRTFun (CallEvalFunc commit) = do
  emitStr "PICC_commit_call_evalfun("
  emitBExpr commit
  emitStr ")"
emitRTFun (RegisterInputCommitment pt chan val cont) = do
  emitStr "PICC_register_input_commitment("
  emitBExpr pt
  emitStr ", "
  emitBExpr chan
  emitStr ", "
  emitBExpr val
  emitStr ", "
  emitBExpr cont
  emitStr ")"
emitRTFun (RegisterOutputCommitment pt chan f cont) = do
  emitStr "PICC_register_output_commitment("
  emitBExpr pt
  emitStr ", "
  emitBExpr chan
  emitStr ", "
  emitBExpr f
  emitStr ", "
  emitBExpr cont
  emitStr ")"
emitRTFun (TryInputAction chan res) = do
  emitStr "PICC_try_input_action("
  emitBExpr chan
  emitStr ", "
  emitStr "&"
  emitBExpr res
  emitStr ")"
emitRTFun (TryOutputAction chan res) = do
  emitStr "PICC_try_output_action("
  emitBExpr chan
  emitStr ", "
  emitStr "&"
  emitBExpr res
  emitStr ")"
emitRTFun (ChannelArrayCreate n) = do
  emitStr "PICC_channelarray_alloc("
  emitBExpr n
  emitStr ")"
emitRTFun (ChannelArrayLockAndRegister chans n pt chan) = do
  emitStr "PICC_channelarray_lock_and_register("
  emitBExpr chans
  emitStr ", "
  emitStr "&"
  emitBExpr n
  emitStr ", "
  emitBExpr pt
  emitStr ", "
  emitBExpr chan
  emitStr ")"
emitRTFun (ChannelArrayUnlock chans n) = do
  emitStr "PICC_channelarray_unlock("
  emitBExpr chans
  emitStr ", "
  emitBExpr n
  emitStr ")"


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
emitVarName PiThread   = emitStr "pt"
emitVarName Child      = emitStr "child"
emitVarName Scheduler  = emitStr "scheduler"
emitVarName (V i)      = emitStr $ "v" ++ show i
emitVarName Chan       = emitStr "chan"
emitVarName Chans      = emitStr "chans"
emitVarName NbChans    = emitStr "nbChans"
emitVarName Commit     = emitStr "commit"
emitVarName TryResult  = emitStr "try_result"
emitVarName Ok         = emitStr "ok"
emitVarName NbDisabled = emitStr "nb_disabled"

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

