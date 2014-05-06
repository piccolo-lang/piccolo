module Back.Backend where

import Back.SeqAST
import Back.RTOptions
import Back.CodeEmitter

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
  void                       :: PiccType a

  -- primitive types (of the target language)
  primBool                   :: PiccType a
  primInt                    :: PiccType a
  primString                 :: PiccType a
  
  -- types of the runtime library
  ptValue                    :: PiccType a
  ptBool                     :: PiccType a
  ptInt                      :: PiccType a
  ptString                   :: PiccType a
  ptChannel                  :: PiccType a
  ptNoValue                  :: PiccType a

  channel                    :: PiccType a
  handle                     :: PiccType a

  schedPool                  :: PiccType a
  piThread                   :: PiccType a

  mutex                      :: PiccType a
  clock                      :: PiccType a

  commit                     :: PiccType a
  inCommit                   :: PiccType a
  outCommit                  :: PiccType a

  pcLabel                    :: PiccType a
  commitList                 :: PiccType a

  knownSet                   :: PiccType a
  knownValue                 :: PiccType a

  queue                      :: PiccType a
  readyQueue                 :: PiccType a
  waitQueue                  :: PiccType a

  pDef                       :: PiccType a
  evalTy                     :: PiccType a
  evalAsVar                  :: VarDescr a

  -- enum type for status and corresponding values
  statusEnum                 :: PiccType a
  statusCall                 :: Expr a
  statusWait                 :: Expr a
  statusEnded                :: Expr a
  statusBlocked              :: Expr a

  -- enum type for try and corresponding values
  tryEnum                    :: PiccType a
  tryEnabled                 :: Expr a
  tryDisabled                :: Expr a
  tryCommit                  :: Expr a

  -- enum type for commit status nd corresponding values
  commitStatusEnum           :: PiccType a
  commitCannotAcquire        :: Expr a
  commitValid                :: Expr a
  commitInvalid              :: Expr a

  -- constant values
  fuelInit                   :: Expr a
  invalidPC                  :: Expr a

  -- value initialization functions
  makeTrue                   :: VarDescr a
  makeFalse                  :: VarDescr a
  makeInt                    :: VarDescr a
  makeString                 :: VarDescr a
  makeChannel                :: VarDescr a

  -- string allocation function
  createStringHandle         :: String -> Expr a

  -- entry point
  dEntry                     :: Expr a
  
  -- some misc values
  null                       :: Value a
  zero                       :: Value a
  primFalse                  :: Value a
  primTrue                   :: Value a
  pcLabelInit                :: Value a
  tryResultInit              :: Expr a


class BackendNames a where
  bnScheduler              :: Name a
  bnPiThread               :: Name a
