{-|
Module         : Middle.Compilation
Description    : Compilation module
Stability      : experimental

This module contains the compilation pass. It transforms a piccolo AST to a sequential AST.
-}
module Middle.Compilation (compilePass) where

import PiccError
import Front.AST
import Front.ASTUtils
import qualified Back.SeqAST as S
import qualified Back.Backend as B
import Back.SeqASTUtils

import Data.List (delete)
import Control.Monad.Error
import Control.Monad.State


data CompState = CompState { locVarCount :: Int
                           , labelCount  :: Int
                           }

type CompilingM a = ErrorT PiccError (State CompState) a

resetLocCount :: CompilingM ()
resetLocCount = do
  st <- get
  put st { locVarCount = 0 }

genLocalVar :: CompilingM (S.VarName a)
genLocalVar = do
  st <- get
  let l = locVarCount st
  put st { locVarCount = l + 1 }
  return $ S.SimpleName (S.Name ("v" ++ show l))

resetLabCount :: CompilingM ()
resetLabCount = do
  st <- get
  put st { labelCount = 0 }

genLabel :: (B.Backend a) => CompilingM (S.Expr a, String)
genLabel = do
  st <- get
  let l = labelCount st
  put st { labelCount = l + 1 }
  return (S.Val (show l, B.labelType), "label" ++ show l)


-- | The 'compilePass' monad run the piccolo AST compilation to sequential AST
compilePass :: (B.Backend a) => Modul -> Either PiccError (S.Instr a)
compilePass m = evalState (runErrorT instr) initEnv
  where instr    = compileModul m
        initEnv  = CompState 0 0


compileModul :: (B.Backend a) => Modul -> CompilingM (S.Instr a)
compileModul m = do
  decls <- forM (modDefs m) $ \def -> do
    return $ S.DeclareFun (S.SimpleName (name def), S.Fun B.voidType [B.schedulerType, B.ptType])
                          (map extractVarName [B.scheduler, B.pt])
                          []
  deffs <- forM (modDefs m) $ \def -> compileDefinition (name def) def
  return $ S.SeqBloc [ S.SeqBloc decls
                     , S.SeqBloc deffs
                     ]
  where name def = S.Name $ (delete '/' $ modName m) ++ "_" ++ defName def

compileDefinition :: (B.Backend a) => S.Name a -> Definition -> CompilingM (S.Instr a)
compileDefinition name def = do
  resetLabCount
  (dEntry, _) <- genLabel
  procInstrs  <- compileProcess (defBody def)
  return $ S.DeclareFun (S.SimpleName name, S.Fun B.voidType [B.schedulerType, B.ptType])
                        (map extractVarName [B.scheduler, B.pt])
                        [ S.Switch (S.Var B.ptPc) [ S.Case dEntry, procInstrs ]]

compileProcess :: (B.Backend a) => Process -> CompilingM (S.Instr a)
compileProcess proc@PEnd    {} = do
  com <- commentProcess proc
  return $ S.SeqBloc [ com
                     , S.ProcCall B.processEnd [S.Var B.pt, S.Val B.statusEnded]
                     , S.Return $ S.Val B.void
                     ]
compileProcess proc@PPrefix {} = do
  pref <- compileAction $ procPref proc
  cont <- compileProcess $ procCont proc
  return $ S.SeqBloc [ pref, cont ]
compileProcess proc@PChoice {} = throwError $ TodoError "compileProcess/PChoice"
compileProcess proc@PCall   {} = do
  com   <- commentProcess proc
  resetLocCount
  loop1 <- forM (procArgs proc) $ \arg -> do
    v <- genLocalVar
    t <- compileType (valTyp arg)
    let vi = (v, t)
    e <- compileValue arg
    return ((vi, arg), S.SeqBloc [ S.DeclareVar vi
                          , e
                          , S.Assign vi (S.Var B.ptVal)
                          ])
  let (vars, instrs1) = unzip loop1
  loop2 <- forM (zip vars [0..]) $ \((vi, arg), i) -> do
    let instr = S.Assign (B.ptEnvI i) (S.Var vi)
    case valTyp arg of
      TChannel {} -> return $ S.SeqBloc [instr,
                                        S.ProcCall B.kRegister [S.Var B.ptKnows, S.Var vi]]
      _       -> return instr
  let name = (delete '/' $ procModule proc) ++ "_" ++ procName proc
  return $ S.SeqBloc [ com
                     , S.ProcCall B.ksForgetAll [S.Var B.ptKnows]
                     , S.SemBloc $ instrs1 ++ loop2 ++
                       [ S.Assign B.ptProc (S.Val (name, B.defType))
                       , S.Assign B.ptPc (S.Val ("0", B.labelType))
                       , S.Assign B.ptStatus (S.Val B.statusCall)
                       , S.Return $ S.Val B.void
                       ]
                     ]

compileBranch :: (B.Backend a) => Branch -> CompilingM (S.Instr a)
compileBranch br = throwError $ TodoError "compileBranch"

compileAction :: (B.Backend a) => Action -> CompilingM (S.Instr a)
compileAction act@AOutput {} = do
  com                    <- commentAction act
  (startLabel, _)        <- genLabel
  (contLabel, contLabel')<- genLabel
  compiledExpr <- compileValue (actData act)
  let ok         = (S.SimpleName (S.Name "ok"), B.boolType)
  let outChan    = (S.SimpleName (S.Name"outchan"), B.channelType)
  let commit     = (S.SimpleName (S.Name "commit"), B.incommitType)
  let tryResult  = (S.SimpleName (S.Name "tryresult"), B.tryResultType)
  return $ S.SeqBloc [ com
                     , S.Case startLabel
                     , S.SemBloc [ S.DeclareVar ok
                                 , S.DeclareVar outChan
                                 , S.Assign outChan (S.Var (B.ptEnvI (actChanIndex act)))
                                 , S.Assign ok
                                   (S.FunCall B.processAcquireChannel
                                     [ S.Var B.pt, S.Var outChan ])
                                 , S.If (S.OpU S.Not (S.Var ok))
                                     [ S.Assign B.ptPc startLabel
                                     , S.ProcCall B.processYield [S.Var B.pt]
                                     , S.Return $ S.Val B.void
                                     ]
                                     []
                                 , S.DeclareVar commit
                                 , S.Assign commit
                                   (S.FunCall B.tryOutputAction
                                     [ S.Var outChan, S.Var tryResult ])
                                 , S.If (S.Op S.Equal (S.Var tryResult) (S.Val B.tryResultEnabled))
                                     [ S.ProcCall B.releaseChannel [ S.Var outChan ]
                                     , S.SeqBloc [compiledExpr]
                                     , S.Assign (S.SimpleName (S.Name "TODO commit.thread.env[commit.refval]"), S.Sty "TODOtype")
                                       (S.Var B.ptVal)
                                     , S.ProcCall B.awake
                                       [S.Var B.scheduler, S.Var (S.SimpleName (S.Name "TODOcommitThread"), S.Sty "TODO type"), S.Var commit]
                                     , S.Goto contLabel'
                                     ]
                                     [S.If (S.Op S.Equal (S.Var tryResult) (S.Val B.tryResultDisabled))
                                       [ S.ProcCall B.releaseChannel [ S.Var outChan ]
                                       , S.ProcCall B.processEnd [ S.Var B.pt, S.Val B.statusBlocked ]
                                       , S.Return $ S.Val B.void
                                       ]
                                       []
                                     ]
                                 , S.ProcCall B.registerOutputCommit
                                   [ S.Var B.pt, S.Var outChan, S.Val ("TODO eval function", S.Sty "TODO type"), contLabel ]
                                 , S.ProcCall B.acquire
                                   [ S.Var B.ptLock ]
                                 , S.ProcCall B.releaseChannel
                                   [ S.Var outChan ]
                                 , S.ProcCall B.processWait
                                   [ S.Var B.pt ]
                                 , S.Return $ S.Val B.void
                                 ]
                     , S.Case contLabel
                     , S.Label contLabel'
                     ]
compileAction act@AInput  {} = throwError $ TodoError "compileAction/AInput"
compileAction act@ANew    {} = do
  com  <- commentAction act
  return $ S.SeqBloc [ com
                     , S.Assign (B.ptEnvI (actBindIndex act)) $
                       S.FunCall B.generateChannel [S.Var B.pt]
                     ]
compileAction act@ALet    {} = do
  com  <- commentAction act
  expr <- compileValue $ actVal act
  return $ S.SeqBloc [ com
                     , expr
                     , S.Assign (B.ptEnvI (actBindIndex act)) $ S.Var B.ptVal
                     ]
compileAction act@ASpawn  {} = do
  com  <- commentAction act
  resetLocCount
  let child = (S.SimpleName (S.Name "child"), B.ptType)
  let name  = (delete '/' $ actModule act) ++ "_" ++ actName act
  instrs <- forM (zip (actArgs act) (map show [0..])) $ \(arg, i) -> do
    v <- genLocalVar
    t <- compileType (valTyp arg)
    let vi = (v, t)
    e <- compileValue arg
    let e' = S.Assign vi (S.Var B.ptVal)
    let e'' = S.Assign (S.SimpleName (S.Name "child.env[i]"), B.valueType) (S.Var vi)
    case valTyp arg of
      TChannel {} -> return $ S.SeqBloc [ e
                                        , e'
                                        , S.ProcCall B.kRegister [S.Var (S.SimpleName (S.Name "child.knows"), S.Sty "TODO"), S.Var vi]
                                        , e''
                                        ]
      _           -> return $ S.SeqBloc [ e, e', e'' ]
  return $ S.SeqBloc [ com
                     , S.SemBloc $
                       [ S.DeclareVar child
                       , S.Assign child (S.FunCall B.generatePiThread [B.convertInt 10])] -- TODO
                        ++ instrs ++
                       [ S.Assign (S.SimpleName (S.Name "TODO childProc"), S.Sty "TODO type") (S.Val (name, B.defType))
                       , S.Assign (S.SimpleName (S.Name "TODO childPc"), S.Sty "TODO type") (S.Val ("0", B.labelType))
                       , S.Assign (S.SimpleName (S.Name "TODO childStatus"), S.Sty "TODO type") (S.Val B.statusRun)
                       , S.ProcCall B.readyQueuePush [S.Val ("TODO scheduler.ready.child", S.Sty "TODO type")]
                       ]
                     ]
compileAction act@APrim   {} = do
  com  <- commentAction act
  resetLocCount
  loop <- forM (actArgs act) $ \arg -> do
    v <- genLocalVar
    t <- compileType (valTyp arg)
    let vi = (v, t)
    e <- compileValue arg
    return (vi, S.SeqBloc [ S.DeclareVar vi
                          , e
                          , S.Assign vi (S.Var B.ptVal)
                          ])
  let (vars, instrs) = unzip loop
  let varExprs = map S.Var vars
  return $ S.SeqBloc [ com
                     , S.SemBloc $ instrs ++
                       [ S.ProcCall (B.makePrim (actModule act) (actName act)) varExprs ]]

compileValue :: (B.Backend a) => Value -> CompilingM (S.Instr a)
compileValue VTrue   {} =
  return $ S.ProcCall B.makeTrue [ S.Var B.ptVal ]
compileValue VFalse  {} =
  return $ S.ProcCall B.makeFalse [ S.Var B.ptVal ]
compileValue val@VInt    {} =
  return $ S.ProcCall B.makeInt [ S.Var B.ptVal, B.convertInt (valInt val) ]
compileValue val@VString {} =
  return $ S.ProcCall B.makeString [ S.Var B.ptVal, B.convertString (valStr val) ]
compileValue val@VVar    {} =
  return $ S.Assign B.ptVal $ S.Var (B.ptEnvI (valIndex val))
compileValue val@VPrim   {} = do
  resetLocCount
  loop <- forM (valArgs val) $ \arg -> do
    v <- genLocalVar
    t <- compileType (valTyp arg)
    let vi = (v, t)
    e <- compileValue arg
    return (vi, S.SeqBloc [ S.DeclareVar vi
                          , e
                          , S.Assign vi (S.Var B.ptVal)
                          ])
  let (vars, instrs) = unzip loop
  let varExprs = map S.Var vars
  return $ S.SemBloc $ instrs ++
    [ S.Assign B.ptVal (S.FunCall (B.makePrim (valModule val) (valName val)) varExprs) ]
compileValue VTuple  {} = throwError $ TodoError "tuples not yet supported"

compileType :: (B.Backend a) => TypeExpr -> CompilingM (S.PiccType a)
compileType _ = return B.valueType


comment :: (B.Backend a) => String -> CompilingM (S.Instr a)
comment str = return $ S.Comment ("---------- " ++ str ++ " ----------")

commentProcess :: (B.Backend a) => Process -> CompilingM (S.Instr a)
commentProcess proc = comment $ procSExpr [] proc

commentAction :: (B.Backend a) => Action -> CompilingM (S.Instr a)
commentAction act = comment $ actSExpr [] act

