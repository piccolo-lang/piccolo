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

import Debug.Trace
import Control.Monad


-- | 'CBackend' is the empty datatype used in the place of the phantom type in sequential AST.
data CBackend

instance BackendConsts CBackend where
  ptDef            = error "TODO CBackend.ptDef"

  pt               = (SimpleName (Name "pt"), Pty "*" (Sty "PICC_PiThread"))
  ptStatus         = (RecordName pt (Name "status"),  Sty "unknown_ptStatus")
  ptEnabled        = (RecordName pt (Name "enabled"), Sty "unknown_ptEnabled")
  ptKnows          = (RecordName pt (Name "knowns"),  Sty "unknown_ptKnows")
  ptEnv            = (RecordName pt (Name "env"),     Sty "unknown_ptEnv")
  ptCommit         = (RecordName pt (Name "commit"),  Sty "unknown_ptCommit")
  ptCommits        = (RecordName pt (Name "commits"), Sty "unknown_ptCommits")
  ptProc           = (RecordName pt (Name "proc"),    Sty "unknwown_ptProc")
  ptPc             = (RecordName pt (Name "pc"),      Sty "unknown_ptPc")
  ptVal            = (RecordName pt (Name "val"),     Sty "PICC_Value")
  ptClock          = (RecordName pt (Name "clock"),   Sty "unknown_ptCLock")
  ptFuel           = (RecordName pt (Name "fuel"),    Sty "unknown_ptFuel")
  ptLock           = (RecordName pt (Name "lock"),    Sty "unknown_ptLock")

  ptEnvI i         = (ArrayName (fst ptEnv) (Val (show i, Sty "int")), Sty "PICC_Value")

  chan             = error "TODO CBackend.chan"
  chanIncommits    = error "TODO CBackend.chanIncommits"
  chanOutcommits   = error "TODO CBackend.chanOutcommits"
  chanGlobalrc     = error "TODO CBackend.chanGlobalrc"
  chanLock         = error "TODO CBackend.chanLock"

  scheduler        = (SimpleName (Name "scheduler"), Pty "*" (Sty "PICC_SchedPool"))

  labelType        = Sty "unknwown_label"
  valueType        = Sty "PICC_Value"
  stringHandleType = Pty "*" (Sty "PICC_StringHandle")
  
  statusType       = Sty "unknown_statusType"
  statusRun        = ("PICC_STATUS_RUN",     statusType)
  statusCall       = ("PICC_STATUS_CALL",    statusType)
  statusWait       = ("PICC_STATUS_WAIT",    statusType)
  statusBlocked    = ("PICC_STATUS_BLOCKED", statusType)
  statusEnded      = ("PICC_STATUS_ENDED",   statusType)

  void             = ("void", Sty "void")

  makeTrue         = (SimpleName (Name "PICC_INIT_BOOL_TRUE"),
                      Fun (Sty "void") [Sty "unknown_makeTrue"])
  makeFalse        = (SimpleName (Name "PICC_INIT_BOOL_FALSE"),
                      Fun (Sty "void") [Sty "unknown_makeFalse"])
  makeInt          = (SimpleName (Name "PICC_INIT_INT_VALUE"),
                      Fun (Sty "void") [Sty "unknown_makeInt"])
  makeString       = (SimpleName (Name "PICC_INIT_STRING_VALUE"),
                      Fun (Sty "void") [Sty "PICC_Value", Sty "unknown_stringHandle"])
  makeStringHandle = (SimpleName (Name "PICC_create_string_handle"),
                      Fun stringHandleType [Pty "*" (Sty "char")])

  convertInt i     = Val (show i, Sty "int")
  convertString s  = FunCall makeStringHandle [Val (s, Pty "*" (Sty "char"))]

  processEnd       = (SimpleName (Name "PICC_ProcessEnd"),
                      Fun (Sty "void") [extractVarType pt, statusType])

  makePrim "core/arith" "add"        = (SimpleName (Name "TODO add"),
                                        Fun valueType [valueType, valueType])
  makePrim "core/arith" "substract"  = (SimpleName (Name "TODO substract"),
                                        Fun valueType [valueType, valueType])
  makePrim "core/arith" "modulo"     = (SimpleName (Name "TODO modulo"),
                                        Fun valueType [valueType, valueType])
  makePrim "core/arith" "equals"     = (SimpleName (Name "TODO equals"),
                                        Fun valueType [valueType, valueType])
  makePrim "core/io"    "print_info" = (SimpleName (Name "TODO print_info"),
                                        Fun valueType [valueType])
  makePrim "core/io"    "print_str"  = (SimpleName (Name "PICC_print_value"),
                                        Fun valueType [valueType])
  makePrim "core/io"    "print_int"  = (SimpleName (Name "PICC_print_value"),
                                        Fun valueType [valueType])
  makePrim _ _                       = error "unknown primitive"

prefixes :: (BackendConsts a) => [(PiccType a, String)]
prefixes = [ (Sty "PICC_Value", "&") ]

emitPrefix :: (BackendConsts a) => PiccType a -> EmitterM ()
emitPrefix t = case lookup t prefixes of
                 Nothing -> return ()
                 Just p  -> emitStr p

instance Backend CBackend where
  emitName (Name n) =
    emitStr n

  emitVarName (SimpleName name)     =
    emitName name
  emitVarName (RecordName var str)  = do
    let (v, t) = var
    emitVarName v
    case t of
      Pty nt _ -> emitStr (if nt == "*" then "->" else ".")
      _        -> emitStr "."
    emitName str
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
        emitName f
        emitStr "("
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
        emitName f
        emitStr "("
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
    let (v, Fun ret argsTyp) = fun
    emitLn ""
    emitIndent
    emitPiccType ret
    emitStr " "
    emitVarName v
    emitStr "("
    emitList (\(v,t) -> do
      emitPiccType t
      emitStr " "
      emitName v) ", " $ zip args argsTyp
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
    emitInstr csq
    decrIndent
    emitLn "} else {"
    incrIndent
    emitInstr alt
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
    emitLn "#include <stdio.h>"
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

