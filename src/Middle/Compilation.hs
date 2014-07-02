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


type LocalVarCount = Int
type LabelCount = Int

-- counter for local ver gen,
-- counter for label gen,
-- backup of the entry label of a definition
type CompilingState = (LocalVarCount, LabelCount, Int)
type CompilingM a = ErrorT PiccError (State CompilingState) a

resetLocCount :: CompilingM ()
resetLocCount = do
  (_, labCount, labEntry) <- get
  put (0, labCount, labEntry)

genLocalVar :: CompilingM (S.VarName a)
genLocalVar = do
  (locCount, labCount, labEntry) <- get
  let v = "v" ++ show locCount
  put (locCount + 1, labCount, labEntry)
  return $ S.SimpleName (S.Name v)

resetLabCount :: CompilingM ()
resetLabCount = do
  (locCount, _, labEntry) <- get
  put (locCount, 0, labEntry)

genLabel :: (B.Backend a) => CompilingM (S.Expr a)
genLabel = do
  (locCount, labCount, labEntry) <- get
  let l = show labCount
  put (locCount, labCount + 1, labEntry)
  return $ S.Val (l, B.labelType)


-- | The 'compilePass' monad run the piccolo AST compilation to sequential AST
compilePass :: (B.Backend a) => Modul -> Either PiccError (S.Instr a)
compilePass m = evalState (runErrorT instr) initEnv
  where instr    = compileModul m
        initEnv  = (0, 0, 0)


compileModul :: (B.Backend a) => Modul -> CompilingM (S.Instr a)
compileModul m = do
  defSigs <- mapM (genFunSig (delete '/' $ modName m)) (modDefs m)
  defDefs <- mapM (compileDefinition (delete '/' $ modName m)) (modDefs m)
  return $ S.SeqBloc [ S.SeqBloc defSigs
                     , S.SeqBloc defDefs
                     ]

genFunSig :: (B.Backend a) => String -> Definition -> CompilingM (S.Instr a)
genFunSig mName def = do
  let args     = [B.scheduler, B.pt]
  let voidType = extractValType B.void
  return $ S.DeclareFun (S.SimpleName n, S.Fun voidType (map extractVarType args))
                        (map extractVarName args)
                        []
  where n = S.Name (mName ++ "_" ++ defName def)

compileDefinition :: (B.Backend a) => String -> Definition -> CompilingM (S.Instr a)
compileDefinition mName def = do
  resetLabCount
  dEntry     <- genLabel
  procInstrs <- compileProcess (defBody def)
  let args     = [B.scheduler, B.pt]
  let voidType = extractValType B.void
  return $ S.DeclareFun (S.SimpleName n, S.Fun voidType (map extractVarType args))
                        (map extractVarName args)
                        [ S.Switch (S.Var B.ptPc) [ S.Case dEntry, procInstrs ]]
  where n = S.Name (mName ++ "_" ++ defName def)


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
compileProcess proc@PCall   {} = throwError $ TodoError "compileProcess/PCall"

compileBranch :: (B.Backend a) => Branch -> CompilingM (S.Instr a)
compileBranch br = throwError $ TodoError "compileBranch"

compileAction :: (B.Backend a) => Action -> CompilingM (S.Instr a)
compileAction act@AOutput {} = throwError $ TodoError "compileAction/AOutput"
compileAction act@AInput  {} = throwError $ TodoError "compileAction/AInput"
compileAction act@ANew    {} = throwError $ TodoError "compileAction/ANew"
compileAction act@ALet    {} = throwError $ TodoError "compileAction/ALet"
compileAction act@ASpawn  {} = throwError $ TodoError "compileAction/ASpawn"
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
compileValue val@VTuple  {} = throwError $ TodoError "tuples not yet supported"

compileType :: (B.Backend a) => TypeExpr -> CompilingM (S.PiccType a)
compileType _ = return B.valueType


comment :: (B.Backend a) => String -> CompilingM (S.Instr a)
comment str = return $ S.Comment ("---------- " ++ str ++ " ----------")

commentProcess :: (B.Backend a) => Process -> CompilingM (S.Instr a)
commentProcess proc = comment $ procSExpr [] proc

commentAction :: (B.Backend a) => Action -> CompilingM (S.Instr a)
commentAction act = comment $ actSExpr [] act

