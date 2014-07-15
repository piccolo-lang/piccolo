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
  emitLn "#include <primitives.h>"
  emitLn ""
  emitDecls instr
  emitLn ""
  emitInstr instr
  emitLn ""
  emitLn "int main() {"
  incrIndent
  emitLn $ "PICC_main(4, " ++ mainName ++ ", 2, 2, 2, 0, 0, 1);"
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
  emitStr "void "
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
emitInstr (ComBloc loc i) = error "ComBloc TODO"
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
emitInstr (Goto lbl)         = emitLn $ "goto label" ++ show lbl
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
    Nop -> do emitStr " else {\n"
              incrIndent
              emitInstr alt
              decrIndent
              emitIndent
              emitStr "}"
    _ -> return ()
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
emitBExpr (FunVal fun)      = error "FunVal TODO"

emitRTFun :: RTFun -> EmitterM ()
emitRTFun (EvalFunc n)                   =
  error "EvalFunc TODO"
emitRTFun (CommitEvalFunc            [pt]) =
  error "CommitEvalFunc TODO"
emitRTFun (PrimCall n (ptVal : vs)) = do
  emitPrimName n
  emitStr "("
  emitStr "&" ; emitBExpr ptVal
  _ <- forM vs $ \v -> do { emitStr ", " ; emitStr "&" ; emitBExpr v }
  emitStr ")"
emitRTFun (GenerateChannel           [pt]) =
  error "GenerateChannel TODO"
emitRTFun (GeneratePiThread          [envSize]) =
  error "GeneratePiThread TODO"
emitRTFun (ProcessWait               [pt]) =
  error "ProcessWait TODO"
emitRTFun (ProcessEnd [pt, status]) = do
  emitStr "PICC_process_end("
  emitBExpr pt
  emitStr ", " ; emitBExpr status
  emitStr ")"
emitRTFun (ProcessYield              [pt]) =
  error "ProcessYield TODO"
emitRTFun (ProcessAcquireChannel     [pt, chan]) =
  error "ProcessAcquireChannel TODO"
emitRTFun (Acquire                   [ptLock]) =
  error "Acquire TODO"
emitRTFun (ReleaseChannel            [chan]) =
  error "ReleaseChannel TODO"
emitRTFun (ReleaseAllChannels        [chans, nbChans]) =
  error "ReleaseAllChannels TODO"
emitRTFun (ChannelRef                [chan]) =
  error "ChannelRef TODO"
emitRTFun (ChannelIncrRefCount       [chan]) =
  error "ChannelIncrRefCount TODO"
emitRTFun (ChannelAcquireAndRegister [pt, chan, chans, nbChans]) =
  error "ChannelAcquireAndRegister TODO"
emitRTFun (Awake                     [sched, commitThread, commit]) =
  error "Awake TODO"
emitRTFun (TryInputAction            [commit, tryResult]) =
  error "TryInputAction TODO"
emitRTFun (TryOutputAction           [commit, tryResult]) =
  error "TryOutputAction TODO"
emitRTFun (RegisterInputCommitment   [pt, chan, x, contLabel]) =
  error "RegisterInputCommitment TODO"
emitRTFun (RegisterOutputCommitment  [pt, chan, efun, contLabel]) =
  error "RegisterOutputCommiment TODO"
emitRTFun (KnowRegister              [ptKnows, chan]) =
  error "KnowRegister TODO"
emitRTFun (KnowSetForgetAll          [ptKnows]) =
  error "KnowSetForgetAll TODO"
emitRTFun (ReadyQueuePush            [schedReady, child]) =
  error "ReadyQueuePush TODO"
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
emitRTFun (BoolFromValue             [ptVal]) =
  error "BoolFromValue TODO"
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
emitType ChannelType       = error "ChannelType TODO"
emitType InCommitType      = error "InCommitType TODO"
emitType OutCommitType     = error "OutCcommitType TODO"
emitType ChannelArrayType  = error "ChannelArrayType TODO"
emitType TryResultEnumType = error "TryResultEnumType TODO"

emitVarName :: VarName -> EmitterM ()
emitVarName PiThread            = emitStr "pt"
emitVarName PiThreadVal         = emitStr "pt->val"
emitVarName PiThreadPC          = emitStr "pt->pc"
emitVarName (PiThreadEnv i)     = emitStr $ "pt->env[" ++ show i ++ "]"
emitVarName PiThreadKnows       = emitStr "pt->knows"
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
emitVarName ChildKnows          = emitStr "child->knows"
emitVarName Scheduler           = emitStr "scheduler"
emitVarName SchedulerReady      = error "SchedulerReady TODO"
emitVarName (V i)               = emitStr $ "v" ++ show i
emitVarName Chan                = emitStr "chan"
emitVarName Chans               = emitStr "chans"
emitVarName NbChans             = emitStr "nbChans"
emitVarName Commit              = emitStr "commit"
emitVarName CommitThread        = error "CommitThread TODO"
emitVarName CommitThreadVal     = error "CommitThreadVal TODO"
emitVarName (CommitThreadEnv i) = error "CommitThreadEnv TODO"
emitVarName TryResult           = emitStr "tryResult"
emitVarName OK                  = emitStr "ok"
emitVarName NbDisabled          = emitStr "nbDisabled"

emitEvalfuncName :: EvalfuncName -> EmitterM ()
emitEvalfuncName (EvalfuncName s) = error "emitEvalfuncName TODO"

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



--instance Backend CBackend where
--  emitVarName (RecordName var str)  = do
--    let (v, t) = var
--    emitVarName v
--    case t of
--      Pty nt _ -> emitStr (if nt == "*" then "->" else ".")
--      _        -> emitStr "."
--    emitStr str
--  emitVarName (ArrayName var expr)  = do
--    emitVarName var
--    emitStr "["
--    emitExpr expr
--    emitStr "]"
--  
--  emitExpr (Op o e1 e2)       = do
--    emitStr "("
--    emitExpr e1
--    emitStr ") "
--    emitBinop o
--    emitStr " ("
--    emitExpr e2
--    emitStr ")"
--  emitExpr (OpU o e)          = do
--    emitUnop o
--    emitStr "("
--    emitExpr e
--    emitStr ")"
--  emitExpr (FunCall fun args) =
--    case fun of
--      (SimpleName f, Fun _ argTypes) -> do
--        emitStr $ f ++ "("
--        emitList (\(expr,typ) -> do { emitPrefix typ ; emitExpr expr })
--          ", " (zip args argTypes)
--        emitStr ")"
--      (f, _) -> do
--        emitVarName f
--        emitStr "("
--        emitList emitExpr ", " args
--        emitStr ")"
--
--  emitBinop Sum   = emitStr "+"
--  emitBinop Minus = emitStr "-"
--  emitBinop Mult  = emitStr "*"
--  emitBinop Div   = emitStr "/"
--  emitBinop Equal = emitStr "=="
--
--  emitUnop Not = emitStr "!"
--
--  emitInstr (Comment str)              = do
--    emitLn ""
--    emitLn $ "/* " ++ str ++ " */"
--  emitInstr (Debug str)                =
--    emitLn $ "printf(\"%s\", " ++ str ++ ");"
--  emitInstr (Switch expr body)         = do
--    emitIndent
--    emitStr "switch ("
--    emitExpr expr
--    emitStr ") {\n"
--    incrIndent
--    emitList emitInstr "" body
--    decrIndent
--    emitIndent
--    emitStr "}\n"
--  emitInstr (Case expr)                = do
--    decrIndent
--    emitIndent
--    emitStr "case "
--    emitExpr expr
--    emitStr ":\n"
--    incrIndent
--  emitInstr (SeqBloc instrs)           =
--    forM_ instrs emitInstr
--  emitInstr (SemBloc instrs)           = do
--    emitLn "{"
--    incrIndent
--    emitList emitInstr "" instrs
--    decrIndent
--    emitLn "}"
--  emitInstr (ComBloc loc instr)        = do
--    emitIndent
--    emitStr $ "#line " ++ show (locStartLine loc) ++ "\n"
--    emitInstr instr
--  emitInstr (ProcCall fun args)        = do
--    emitIndent
--    case fun of
--      (SimpleName f, Fun _ argTypes) -> do
--        emitStr $ f ++ "("
--        emitList (\(expr,typ) -> do { emitPrefix typ ; emitExpr expr })
--          ", " (zip args argTypes)
--        emitStr ");\n"
--      (f, _) -> do
--        emitVarName f
--        emitStr "("
--        emitList emitExpr ", " args
--        emitStr ");\n"
--  emitInstr (DeclareVar (var, typ))    = do
--    emitIndent
--    emitPiccType typ
--    emitStr " "
--    emitVarName var
--    emitStr ";\n"
--  emitInstr (Assign (var, _) expr)     = do
--    emitIndent
--    emitVarName var
--    emitStr " = "
--    emitExpr expr
--    emitStr ";\n"
--  emitInstr (ForEach (var, _) expr body) = do
--    emitLn "{"
--    incrIndent
--    emitIndent
--    emitStr "PICC_KnownValue "
--    emitVarName var
--    emitStr ";\n"
--    emitIndent
--    emitStr "PICC_KnownSet* s = "
--    emitExpr expr
--    emitStr ";\n"
--    emitIndent
--    emitStr "PICC_KNOWNSET_FOREACH(s, "
--    emitVarName var
--    emitStr ") {\n"
--    incrIndent
--    emitIndent
--    emitInstr body
--    decrIndent
--    emitLn "}"
--    emitLn "PICC_free_knownset(s);"
--    decrIndent
--    emitLn "}"
--  emitInstr (If cond csq alt)          = do
--    emitIndent
--    emitStr "if ("
--    emitExpr cond
--    emitStr ") {\n"
--    incrIndent
--    emitInstr $ SeqBloc csq
--    decrIndent
--    emitLn "} else {"
--    incrIndent
--    emitInstr $ SeqBloc alt
--    decrIndent
--    emitLn "}"
--  emitInstr (DoWhile body expr)        = do
--    emitLn "do {"
--    incrIndent
--    emitInstr body
--    decrIndent
--    emitIndent
--    emitStr "} while ("
--    emitExpr expr
--    emitStr ");\n"
--  
--
