{-|
module         : Backend.SeqAST
Description    : The sequential AST for code generation
Stability      : experimental

Piccolo-core language is compiled into a sequential AST allowing sequential
code generation. This module describes the structure of this AST.
-}
module Piccolo.SeqAST
  ( DefName (..)
  , PrimName (..)
  , EvalfuncName (..)
  , VarName (..)
  , Type (..)
  , EnumName (..)
  , RTFun (..)
  , BExpr (..)
  , Lab
  , Instr (..)
  )
where

import qualified Piccolo.AST as PiAST

-- | Definition corresponding to a compiled process definition
data DefName = DefName [String] String

-- | Primitives
data PrimName = PrimName [String] String

-- | Evaluation function for computing output payload
newtype EvalfuncName = EvalfuncName Int

-- | Reserved variable names of runtime
data VarName
  = PiThread
  | Child
  | Scheduler
  | V Int
  | Chan
  | Chans
  | NbChans
  | Commit
  | TryResult
  | Ok
  | NbDisabled

-- | Runtime types
data Type
  = BoolType
  | IntType
  | PiValueType
  | PiThreadType
  | SchedulerType
  | ChannelType
  | CommitType
  | ChannelArrayType
  | TryResultEnumType

-- | Runtime enumerations
data EnumName
  = StatusRun
  | StatusBlocked
  | StatusCall
  | StatusEnded
  | TryResultEnabled
  | TryResultDisabled
  | TryResultAbort

-- | Runtime functions
data RTFun
  = EvalFunc EvalfuncName      -- ^ Evaluation function for output payload
  | PrimCall PrimName [BExpr]  -- ^ Primitive
  | DefProc DefName            -- ^ Compiler process definition

  -- Pithreads related functions
  | PiThreadCreate              BExpr BExpr
  | SetProc                     BExpr BExpr
  | GetPC                       BExpr
  | SetPC                       BExpr BExpr
  | GetRegister                 BExpr
  | SetRegister                 BExpr BExpr
  | RegisterPointer             BExpr
  | GetEnv                      BExpr BExpr
  | SetEnv                      BExpr BExpr BExpr
  | GetEnabled                  BExpr BExpr
  | SetEnabled                  BExpr BExpr BExpr
  | SetStatus                   BExpr BExpr
  | SetSafeChoice               BExpr BExpr
  | GetFuel                     BExpr
  | DecrFuel                    BExpr
  | ForgetAllValues             BExpr
  | RegisterEnvValue            BExpr BExpr
  | ProcessLock                 BExpr
  | ProcessLockChannel          BExpr BExpr
  | ProcessYield                BExpr BExpr
  | ProcessWait                 BExpr BExpr
  | ProcessAwake                BExpr BExpr
  | ProcessEnd                  BExpr BExpr

  -- Values related functions
  | InitNoValue                 BExpr
  | InitBoolTrue                BExpr
  | InitBoolFalse               BExpr
  | InitIntValue                BExpr BExpr
  | InitFloatValue              BExpr BExpr
  | InitStringValue             BExpr BExpr
  | InitChannelValue            BExpr
  | UnboxChannelValue           BExpr
  | UnboxBoolValue              BExpr
  | UnlockChannel               BExpr

  -- Scheduler related functions
  | ReadyPushFront              BExpr BExpr
  | SchedGetReadyQueue          BExpr

  -- Commitments related functions
  | GetThread                   BExpr
  | GetRefVar                   BExpr
  | CallEvalFunc                BExpr
  | RegisterInputCommitment     BExpr BExpr BExpr BExpr
  | RegisterOutputCommitment    BExpr BExpr BExpr BExpr

  -- Actions related functions
  | TryInputAction              BExpr BExpr
  | TryOutputAction             BExpr BExpr
  | ChannelArrayCreate          BExpr
  | ChannelArrayLockAndRegister BExpr BExpr BExpr BExpr
  | ChannelArrayUnlock          BExpr BExpr


-- | Runtime expressions
data BExpr
  = Not BExpr
  | Equal BExpr BExpr
  | BoolExpr Bool
  | IntExpr Int
  | FloatExpr Float
  | StringExpr String
  | Var VarName
  | Enum EnumName
  | FunCall RTFun       -- ^ Function call
  | FunVal RTFun        -- ^ Function as a value

-- | Label type for program counter
type Lab = Int

-- | Sequential instructions
data Instr
  = Comment String
  | Seq Instr Instr
  | LexBloc Instr
  | ComBloc PiAST.Location Instr
  | Nop
  | DefFunction DefName ((VarName, Type), (VarName, Type)) Instr
  | EvalFunction EvalfuncName (VarName, Type) Instr
  | DeclareVar VarName Type
  | Assign VarName BExpr
  | Return
  | ReturnRegister
  | Goto Int
  | Increment VarName
  | Decrement VarName
  | If BExpr Instr Instr
  | Switch BExpr Instr
  | Case Int
  | CaseAndLabel Int
  | ProcCall RTFun
