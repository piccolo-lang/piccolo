{-# LANGUAGE EmptyDataDecls #-}
{-|
Module         : Back.CBackend
Description    : A C backend module
Stability      : experimental

This backend targets C code.
-}
module Back.CBackend where

import Front.AST
import Back.SeqAST
import Back.SeqASTUtils
import Back.Backend
import Back.CodeEmitter

import Control.Monad


-- | 'CBackend' is the empty datatype used in the place of the phantom type in sequential AST.
data CBackend

instance BackendTypes CBackend where
  voidType              = Sty "void"
  boolType              = Sty "char"
  intType               = Sty "int"
  stringType            = Pty "*" (Sty "char")
  labelType             = Sty "int"
  channelType           = Pty "*" (Sty "PICC_Channel")

  defType               = stringType

  ptType                = Pty "*" (Sty "PICC_PiThread")
  knowSetType           = Pty "*" (Sty "PICC_KnownSet")
  valueType             = Sty ("PICC_Value")
  statusType            = Sty "PICC_StatusKind"
  tryResultType         = Sty "PICC_TryResult"
  incommitType          = Sty "PICC_InCommit"
  outcommitType         = Sty "PICC_OutCommit"
  readyQueueType        = Pty "*" (Sty "PICC_ReadyQueue")

  schedulerType         = Pty "*" (Sty "PICC_SchedPool")

  stringHandleType      = Pty "*" (Sty "PICC_StringHandle")


instance BackendNames CBackend where
  pt                    = (SimpleName "pt",         ptType)
  ptStatus aPt          = (RecordName aPt "status",  statusType)
  ptEnabled aPt         = (RecordName aPt "enabled", Sty "TODOunknown_ptEnabled")
  ptKnows aPt           = (RecordName aPt "knowns",  knowSetType)
  ptEnv aPt             = (RecordName aPt "env",     Sty "TODOunknown_ptEnv")
  ptCommit aPt          = (RecordName aPt "commit",  Sty "TODOunknown_ptCommit")
  ptCommits aPt         = (RecordName aPt "commits", Sty "TODOunknown_ptCommits")
  ptProc aPt            = (RecordName aPt "proc",    Sty "TODOunknwown_ptProc")
  ptPc aPt              = (RecordName aPt "pc",      labelType)
  ptVal aPt             = (RecordName aPt "val",     valueType)
  ptClock aPt           = (RecordName aPt "clock",   Sty "TODOunknown_ptCLock")
  ptFuel aPt            = (RecordName aPt "fuel",    Sty "TODOunknown_ptFuel")
  ptLock aPt            = (RecordName aPt "lock",    Sty "TODOunknown_ptLock")

  ptEnvI aPtEnv i       = (ArrayName (fst aPtEnv) (convertInt i), valueType)

  chan                  = error "TODO CBackend.chan"
  chanIncommits         = error "TODO CBackend.chanIncommits"
  chanOutcommits        = error "TODO CBackend.chanOutcommits"
  chanGlobalrc          = error "TODO CBackend.chanGlobalrc"
  chanLock              = error "TODO CBackend.chanLock"

  scheduler             = (SimpleName "scheduler", schedulerType)
  schedulerReady aSched = (RecordName aSched "ready", readyQueueType)
  
  statusRun             = ("PICC_STATUS_RUN",     statusType)
  statusCall            = ("PICC_STATUS_CALL",    statusType)
  statusWait            = ("PICC_STATUS_WAIT",    statusType)
  statusBlocked         = ("PICC_STATUS_BLOCKED", statusType)
  statusEnded           = ("PICC_STATUS_ENDED",   statusType)

  tryResultDisabled     = ("PICC_TRYRESULT_DISABLED", tryResultType)
  tryResultEnabled      = ("PICC_TRYRESULT_ENABLED",  tryResultType)
  tryResultCommit       = ("PICC_TRYRESULT_COMMIT",   tryResultType)

  ksForgetAll           = (SimpleName "PICC_knownset_forget_all",
                           Fun voidType [knowSetType])
  kRegister             = (SimpleName "KnowRegister",
                           Fun voidType [knowSetType, channelType])

  void                  = ("void", voidType)

  makeTrue              = (SimpleName "PICC_INIT_BOOL_TRUE",
                           Fun voidType [valueType])
  makeFalse             = (SimpleName "PICC_INIT_BOOL_FALSE",
                           Fun voidType [valueType])
  makeInt               = (SimpleName "PICC_INIT_INT_VALUE",
                           Fun voidType [valueType, intType])
  makeString            = (SimpleName "PICC_INIT_STRING_VALUE",
                           Fun voidType [valueType, stringHandleType])
  makeStringHandle      = (SimpleName "PICC_create_string_handle",
                           Fun stringHandleType [stringType])

  convertInt i          = Val (show i, intType)
  convertString s       = FunCall makeStringHandle [Val (s, stringType)]

  processEnd            = (SimpleName "PICC_process_end",
                           Fun voidType [ptType, statusType])
  processAcquireChannel = error "TODO processAcquireChannel"
  processYield          = error "TODO processYield"
  processWait           = error "TODO processWait"
  awake                 = error "TODO awake"
  generatePiThread      = (SimpleName "PICC_create_pithread",
                           Fun ptType [intType])

  generateChannel       = (SimpleName "PICC_GenerateChannel",
                           Fun valueType [ptType])
  releaseChannel        = error "TODO releaseChannel"
  releaseAllChannels    = error "TODO releaseAllChannels"
  acquire               = error "TODO acquire"

  readyQueuePush        = (SimpleName "PICC_ready_queue_push",
                           Fun voidType [readyQueueType, ptType])

  tryOutputAction       = error "TODO tryOutputAction"
  tryInputAction        = error "TODO tryInputAction"
  registerOutputCommit  = error "TODO registerOutputCommit"
  registerInputCommit   = error "TODO registerInputCommit"


instance BackendPrimitives CBackend where
  makePrim "core/arith" "add"        = (SimpleName "PICC_Int_add",
                                        Fun valueType [valueType, valueType])
  makePrim "core/arith" "substract"  = (SimpleName "PICC_Int_substract",
                                        Fun valueType [valueType, valueType])
  makePrim "core/arith" "modulo"     = (SimpleName "PICC_Int_modulo",
                                        Fun valueType [valueType, valueType])
  makePrim "core/arith" "equals"     = (SimpleName "PICC_equals",
                                        Fun valueType [valueType, valueType])
  makePrim "core/io"    "print_info" = (SimpleName "PICC_print_info",
                                        Fun valueType [valueType])
  makePrim "core/io"    "print_str"  = (SimpleName "PICC_print_value",
                                        Fun valueType [valueType])
  makePrim "core/io"    "print_int"  = (SimpleName "PICC_print_value",
                                        Fun valueType [valueType])
  makePrim _ _                       = error "unknown primitive"


prefixes :: (Backend a) => [(PiccType a, String)]
prefixes = [ (Sty "PICC_Value", "&") ]

emitPrefix :: (Backend a) => PiccType a -> EmitterM ()
emitPrefix t = case lookup t prefixes of
                 Nothing -> return ()
                 Just p  -> emitStr p

instance Backend CBackend where
  emitVarName (SimpleName name)     =
    emitStr name
  emitVarName (RecordName var str)  = do
    let (v, t) = var
    emitVarName v
    case t of
      Pty nt _ -> emitStr (if nt == "*" then "->" else ".")
      _        -> emitStr "."
    emitStr str
  emitVarName (ArrayName var expr)  = do
    emitVarName var
    emitStr "["
    emitExpr expr
    emitStr "]"
  
  emitPiccType (Sty str)      =
    emitStr str
  emitPiccType (Pty str typ)  =
    if str == "*"
      then do { emitPiccType typ ; emitStr "*" }
      else do { emitStr str ; emitPiccType typ }
  emitPiccType (Fun fun args) = do
    emitPiccType fun
    emitList emitPiccType ", " args

  emitExpr (Val (val, _))     = emitStr val
  emitExpr (Var (var, _))     = emitVarName var
  emitExpr (Op o e1 e2)       = do
    emitStr "("
    emitExpr e1
    emitStr ") "
    emitBinop o
    emitStr " ("
    emitExpr e2
    emitStr ")"
  emitExpr (OpU o e)          = do
    emitUnop o
    emitStr "("
    emitExpr e
    emitStr ")"
  emitExpr (FunCall fun args) =
    case fun of
      (SimpleName f, Fun _ argTypes) -> do
        emitStr $ f ++ "("
        emitList (\(expr,typ) -> do { emitPrefix typ ; emitExpr expr })
          ", " (zip args argTypes)
        emitStr ")"
      (f, _) -> do
        emitVarName f
        emitStr "("
        emitList emitExpr ", " args
        emitStr ")"

  emitBinop Sum   = emitStr "+"
  emitBinop Minus = emitStr "-"
  emitBinop Mult  = emitStr "*"
  emitBinop Div   = emitStr "/"
  emitBinop Equal = emitStr "=="

  emitUnop Not = emitStr "!"

  emitInstr (Comment str)              = do
    emitLn ""
    emitLn $ "/* " ++ str ++ " */"
  emitInstr (Debug str)                =
    emitLn $ "printf(\"%s\", " ++ str ++ ");"
  emitInstr (Switch expr body)         = do
    emitIndent
    emitStr "switch ("
    emitExpr expr
    emitStr ") {\n"
    incrIndent
    emitList emitInstr "" body
    decrIndent
    emitIndent
    emitStr "}\n"
  emitInstr (Case expr)                = do
    decrIndent
    emitIndent
    emitStr "case "
    emitExpr expr
    emitStr ":\n"
    incrIndent
  emitInstr (SeqBloc instrs)           =
    forM_ instrs emitInstr
  emitInstr (SemBloc instrs)           = do
    emitLn "{"
    incrIndent
    emitList emitInstr "" instrs
    decrIndent
    emitLn "}"
  emitInstr (ComBloc loc instr)        = do
    emitIndent
    emitStr $ "#line " ++ show (locStartLine loc) ++ "\n"
    emitInstr instr
  emitInstr (ProcCall fun args)        = do
    emitIndent
    case fun of
      (SimpleName f, Fun _ argTypes) -> do
        emitStr $ f ++ "("
        emitList (\(expr,typ) -> do { emitPrefix typ ; emitExpr expr })
          ", " (zip args argTypes)
        emitStr ");\n"
      (f, _) -> do
        emitVarName f
        emitStr "("
        emitList emitExpr ", " args
        emitStr ");\n"
  emitInstr (DeclareVar (var, typ))    = do
    emitIndent
    emitPiccType typ
    emitStr " "
    emitVarName var
    emitStr ";\n"
  emitInstr (Assign (var, _) expr)     = do
    emitIndent
    emitVarName var
    emitStr " = "
    emitExpr expr
    emitStr ";\n"
  emitInstr (DeclareFun fun args body) = do
    let (vt, Fun ret argsTyp) = fun
    emitLn ""
    emitIndent
    emitPiccType ret
    emitStr " "
    emitVarName vt
    emitStr "("
    emitList (\(v,t) -> do
      emitPiccType t
      emitStr " "
      emitStr v) ", " $ zip args argsTyp
    emitStr ")"
    if null body
      then emitStr ";\n"
      else do
        emitStr " {\n"
        incrIndent
        forM_ body emitInstr
        decrIndent
        emitStr "}\n"
  emitInstr (ForEach (var, _) expr body) = do
    emitLn "{"
    incrIndent
    emitIndent
    emitStr "PICC_KnownValue "
    emitVarName var
    emitStr ";\n"
    emitIndent
    emitStr "PICC_KnownSet* s = "
    emitExpr expr
    emitStr ";\n"
    emitIndent
    emitStr "PICC_KNOWNSET_FOREACH(s, "
    emitVarName var
    emitStr ") {\n"
    incrIndent
    emitIndent
    emitInstr body
    decrIndent
    emitLn "}"
    emitLn "PICC_free_knownset(s);"
    decrIndent
    emitLn "}"
  emitInstr (If cond csq alt)          = do
    emitIndent
    emitStr "if ("
    emitExpr cond
    emitStr ") {\n"
    incrIndent
    emitInstr $ SeqBloc csq
    decrIndent
    emitLn "} else {"
    incrIndent
    emitInstr $ SeqBloc alt
    decrIndent
    emitLn "}"
  emitInstr (Label lbl)                = emitLn $ lbl ++ ":"
  emitInstr (Goto lbl)                 = emitLn $ "goto " ++ lbl
  emitInstr (Return (Val ("void", _))) = emitLn "return;"
  emitInstr (Return expr)              = do
    emitIndent
    emitStr "return ("
    emitExpr expr
    emitStr ");\n"
  emitInstr (DoWhile body expr)        = do
    emitLn "do {"
    incrIndent
    emitInstr body
    decrIndent
    emitIndent
    emitStr "} while ("
    emitExpr expr
    emitStr ");\n"
  
  emitCode mainName instr = do
    emitLn "#include <runtime.h>"
    emitLn "#include <value.h>"
    emitLn "#include <queue.h>"
    emitLn "#include <pi_thread_repr.h>"
    emitLn "#include <knownset_repr.h>"
    emitLn "#include <commit_repr.h>"
    emitLn "#include <scheduler_repr.h>"
    emitLn ""
    emitInstr instr
    emitLn ""
    emitLn "int main() {"
    incrIndent
    emitLn $ "PICC_main(4, " ++ mainName ++ ", 2, 2, 2, 0, 0, 1);"
    emitLn "return 0;"
    decrIndent
    emitLn "}"

