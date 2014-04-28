module Back.Backend where

import Back.SeqAST
import Back.RTOptions
import Utils.CodeEmitter

class (BackendTypes a, BackendNames a) => Backend a where
  emitName      :: Name a     -> EmitterM ()
  emitVarName   :: VarName a  -> EmitterM ()
  emitPiccType  :: PiccType a -> EmitterM ()
  emitExpr      :: Expr a     -> EmitterM ()
  emitBinop     :: Binop a    -> EmitterM ()
  emitUnop      :: Unop a     -> EmitterM ()
  emitInstr     :: Instr a    -> EmitterM ()
  emitCase      :: Case a     -> EmitterM ()
  emitCode      :: RTOptions -> String -> Instr a -> EmitterM ()

class BackendTypes a where
  btProcDef                :: PiccType a

class BackendNames a where
  bnScheduler              :: Name a
  bnPiThread               :: Name a
