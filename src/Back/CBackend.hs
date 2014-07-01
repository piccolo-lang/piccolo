{-# LANGUAGE EmptyDataDecls #-}
{-|
Module         : Back.CBackend
Description    : A C backend module
Stability      : experimental

This backend targets C code.
-}
module Back.CBackend where

import Back.SeqAST
import Back.Backend
import Back.CodeEmitter

import Control.Monad


-- | 'CBackend' is the empty datatype used in the place of the phantom type in sequential AST.
data CBackend

instance BackendConsts CBackend where
  ptDef            = error "TODO CBackend.ptDef"

  pt               = (SimpleName (Name "pt"), Sty "pt")
  ptStatus         = error "TODO CBackend.ptStatus"
  ptEnabled        = error "TODO CBackend.ptEnabled"
  ptKnows          = error "TODO CBackend.ptKnows"
  ptEnv            = error "TODO CBackend.ptEnv"
  ptCommit         = error "TODO CBackend.ptCommit"
  ptCommits        = error "TODO CBackend.ptCommits"
  ptProc           = error "TODO CBackend.ptProc"
  ptPc             = error "TODO CBackend.ptPc"
  ptVal            = error "TODO CBackend.ptVal"
  ptClock          = error "TODO CBackend.ptClock"
  ptFuel           = error "TODO CBackend.ptFuel"
  ptLock           = error "TODO CBackend.ptLock"

  ptEnvI           = error "TODO CBackend.ptEnvI"

  chan             = error "TODO CBackend.chan"
  chanIncommits    = error "TODO CBackend.chanIncommits"
  chanOutcommits   = error "TODO CBackend.chanOutcommits"
  chanGlobalrc     = error "TODO CBackend.chanGlobalrc"
  chanLock         = error "TODO CBackend.chanLock"

  scheduler        = (SimpleName (Name "scheduler"), Sty "scheduler")

  labelType        = error "TODO CBackend.labelType"
  
  refBool          = error "TODO CBackend.refBool"
  refInt           = error "TODO CBackend.refInt"
  refString        = error "TODO CBackend.refString"

  defProcType      = Fun (Sty "void") [Sty "scheduler", Sty "pithread"]

  statusRun        = error "TODO CBackend.statusRun"
  statusCall       = error "TODO CBackend.statusCall"
  statusWait       = error "TODO CBackend.statusWait"
  statusEnded      = error "TODO CBackend.statusEnded"

  void             = error "TODO CBackend.void"

  makeTrue         = error "TODO CBackend.makeTrue"
  makeFalse        = error "TODO CBackend.makeFalse"
  makeInt          = error "TODO CBackend.makeInt"
  makeString       = error "TODO CBackend.makeString"
  makePrim         = error "TODO CBackend.makePrim"

  convertInt       = error "TODO CBackend.convertInt"
  convertString    = error "TODO CBackend.convertString"

  processEnd       = error "TODO CBackend.processEnd"


instance Backend CBackend where
  emitName (Name str) = emitStr str

  emitVarName (SimpleName name)     =
    emitName name
  emitVarName (RecordName var str)  = do
    let (v, t) = var
    emitVarName v
    case t of
      Pty nt _ -> if nt == "*"
                       then emitStr "->"
                       else emitStr "."
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
  emitExpr (FunCall fun args) = error "TODO CBackend.emitExpr/FunCall"

  emitBinop Sum   = emitStr "+"
  emitBinop Minus = emitStr "-"
  emitBinop Mult  = emitStr "*"
  emitBinop Div   = emitStr "/"
  emitBinop Equal = emitStr "=="

  emitUnop Not = emitStr "!"

  emitInstr (Comment str)              =
    emitLn $ "/* " ++ str ++ " */"
  emitInstr (Debug str)                =
    emitLn $ "printf(\"%s\", " ++ str ++ ");"
  emitInstr (Switch expr body)         = do
    emitIndent
    emitStr "switch("
    emitExpr expr
    emitStr ") {\n"
    incrIndent
    emitInstr body
    decrIndent
    emitIndent
    emitStr "}\n"
  emitInstr (Case expr body)           = error "TODO CBackend.emitInstr Case"
  emitInstr (SeqBloc instrs)           =
    forM_ instrs emitInstr
  emitInstr (SemBloc instrs)           = error "TODO CBackend.emitInstr SemBloc"
  emitInstr (ComBloc loc instr)        = error "TODO CBackend.emitInstr ComBloc"
  emitInstr (ProcCall fun args)        = error "TODO CBackend.emitInstr ProcCall"
  emitInstr (DeclareVar var)           = error "TODO CBackend.emitInstr DeclareVar"
  emitInstr (Assign var expr)          = error "TODO CBackend.emitInstr Assign"
  emitInstr (DeclareFun fun args body) = do
    let (v, (Fun ret argsTyp)) = fun
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
        emitStr "{\n"
        incrIndent
        forM_ body emitInstr
        decrIndent
        emitStr "}\n"
  emitInstr (ForEach var expr body)    = error "TODO CBackend.emitInstr ForEach"
  emitInstr (If cond csq alt)          = error "TODO CBackend.emitInstr If"
  emitInstr (Label lbl)                = error "TODO CBackend.emitInstr Label"
  emitInstr (Goto lbl)                 = error "TODO CBackend.emitInstr Goto"
  emitInstr (Return expr)              = error "TODO CBackend.emitInstr Return"
  emitInstr (DoWhile expr body)        = error "TODO CBackend.emitInstr DoWhile"
  
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
    emitLn $ "PICC_main(2, " ++ mainName ++ ", 10, 10, 10, 10, 10, 0);"
    emitLn "return 0;"
    decrIndent
    emitLn "}"

