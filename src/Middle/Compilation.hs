{-|
Module         : Middle.Compilation
Description    : Compilation module
Stability      : experimental

This module contains the compilation pass. It transforms a piccolo AST to a sequential AST.
-}
module Middle.Compilation (compilePass) where

import PiccError
import Front.AST
import qualified Back.SeqAST as S
import qualified Back.Backend as B
import Back.SeqASTUtils

import Control.Monad.Error
import Control.Monad.State


type LocalVarCount = Int
type LabelCount = Int

type CompilingState = (LocalVarCount, LabelCount)
type CompilingM a = ErrorT PiccError (State CompilingState) a

resetLocCount :: CompilingM ()
resetLocCount = do
  (_, labCount) <- get
  put (0, labCount)

genLocalVar :: CompilingM (S.VarName a)
genLocalVar = do
  (locCount, labCount) <- get
  let v = "v" ++ show locCount
  put (locCount + 1, labCount)
  return $ S.SimpleName (S.Name v)

resetLabCount :: CompilingM ()
resetLabCount = do
  (locCount, _) <- get
  put (locCount, 0)

genLabel :: (B.Backend a) => CompilingM (S.Expr a)
genLabel = do
  (locCount, labCount) <- get
  let l = "label" ++ show labCount
  put (locCount, labCount + 1)
  return $ S.Val (l, B.labelType)


-- | The 'compilePass' monad run the piccolo AST compilation to sequential AST
compilePass :: (B.Backend a) => Modul -> Either PiccError (S.Instr a)
compilePass m = evalState (runErrorT instr) init
  where instr = compileModul m
        init  = (0, 0)


compileModul :: (B.Backend a) => Modul -> CompilingM (S.Instr a)
compileModul m = do
  defSigs <- mapM (genFunSig (modName m)) (modDefs m)
  defDefs <- mapM (compileDefinition (modName m)) (modDefs m)
  return $ S.SeqBloc [ S.SeqBloc defSigs
                     , S.SeqBloc defDefs
                     ]

genFunSig :: (B.Backend a) => String -> Definition -> CompilingM (S.Instr a)
genFunSig mName def =
  return $ S.DeclareFun (S.SimpleName n, B.defProcType)
                        (map extractName [B.scheduler, B.pt])
                        []
  where n = S.Name (mName ++ "_" ++ defName def)

compileDefinition :: (B.Backend a) => String -> Definition -> CompilingM (S.Instr a)
compileDefinition mName def = do
  resetLabCount
  dEntry     <- genLabel
  procInstrs <- compileProcess (defBody def)
  return $ S.DeclareFun (S.SimpleName (S.Name mName), B.defProcType)
                        (map extractName [B.scheduler, B.pt])
                        [ S.Switch (S.Var B.ptPc)
                          (S.SemBloc [ S.Case dEntry procInstrs ])
                        ]

compileProcess :: (B.Backend a) => Process -> CompilingM (S.Instr a)
compileProcess proc@PEnd    {} =
  return $ S.SeqBloc [ S.ProcCall B.processEnd [S.Var B.pt, S.Val B.statusEnded]
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
  resetLocCount
  loop <- forM (actArgs act) $ \arg -> do
    v <- genLocalVar
    t <- compileType (valTyp arg)
    let vi = (v, t)
    e <- compileValue arg
    return $ (vi, S.SeqBloc [ S.DeclareVar vi
                           , e
                           , S.Assign vi (S.Var B.ptVal)
                           ])
  let (vars, instrs) = unzip loop
  let varExprs = map S.Var vars
  return $ S.SemBloc $ instrs ++
      [ S.ProcCall (B.makePrim (actModule act) (actName act)) varExprs ]

compileValue :: (B.Backend a) => Value -> CompilingM (S.Instr a)
compileValue val@VTrue   {} =
  return $ S.Assign B.ptVal $ S.FunCall B.makeTrue []
compileValue val@VFalse  {} =
  return $ S.Assign B.ptVal $ S.FunCall B.makeFalse []
compileValue val@VInt    {} =
  return $ S.Assign B.ptVal $ S.FunCall B.makeInt [S.Val (B.convertInt (valInt val))]
compileValue val@VString {} =
  return $ S.Assign B.ptVal $ S.FunCall B.makeString [S.Val (B.convertString (valStr val))]
compileValue val@VVar    {} =
  return $ S.Assign B.ptVal $ S.Var (B.ptEnvI (valIndex val))
compileValue val@VPrim   {} = do
  resetLocCount
  loop <- forM (valArgs val) $ \arg -> do
    v <- genLocalVar
    t <- compileType (valTyp arg)
    let vi = (v, t)
    e <- compileValue arg
    return $ (vi, S.SeqBloc [ S.DeclareVar vi
                           , e
                           , S.Assign vi (S.Var B.ptVal)
                           ])
  let (vars, instrs) = unzip loop
  let varExprs = map S.Var vars
  return $ S.SemBloc $ instrs ++
    [ S.Assign B.ptVal (S.FunCall (B.makePrim (valModule val) (valName val)) varExprs) ]

compileType :: (B.Backend a) => TypeExpr -> CompilingM (S.PiccType a)
compileType typ@TUnknown {} = throwError $ SimpleError "unknown type"
compileType typ@TAtom    {} = case typAtom typ of
  TBool   -> return $ B.refBool
  TInt    -> return $ B.refInt
  TString -> return $ B.refString
compileType typ@TChannel {} = throwError $ TodoError "compileType TChannel"
compileType typ@TTuple   {} = throwError $ TodoError "compileType TTuple"
compileType typ@TPrim    {} = throwError $ TodoError "compileType TPrim"

