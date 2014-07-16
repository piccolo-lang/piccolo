{-|
module         : Back.SeqAST
Description    : The sequential AST for code generation
Stability      : experimental

Longer description.
-}
module Back.SeqAST where

import qualified Front.AST as PilAST

data DefName  = DefName  String String
data PrimName = PrimName String String
data EvalfuncName = EvalfuncName Int

data VarName
  = PiThread
  | PiThreadVal
  | PiThreadPC
  | PiThreadEnv Int
  | PiThreadKnows
  | PiThreadLock
  | PiThreadFuel
  | PiThreadProc
  | PiThreadStatus
  | PiThreadEnabled Int
  | Child
  | ChildPC
  | ChildStatus
  | ChildProc
  | ChildEnv Int
  | ChildKnows
  | Scheduler
  | SchedulerReady
  | V Int
  | Chan
  | Chans
  | NbChans
  | Commit
  | CommitThread
  | CommitThreadVal
  | CommitThreadEnv VarName
  | CommitVal
  | CommitRefval
  | TryResult
  | OK
  | NbDisabled

data Type
  = BoolType
  | IntType
  | PiValueType
  | PiThreadType
  | SchedulerType
  | ChannelType
  | InCommitType
  | OutCommitType
  | ChannelArrayType
  | TryResultEnumType

data EnumName
  = StatusRun
  | StatusBlocked
  | StatusCall
  | StatusEnded
  | TryResultEnabled
  | TryResultDisabled
  | TryResultAbort

data RTFun
  = EvalFunc EvalfuncName
  | CommitEvalFunc            [BExpr]
  | PrimCall PrimName         [BExpr]
  | GenerateChannel           [BExpr]
  | GeneratePiThread          [BExpr]
  | ProcessWait               [BExpr]
  | ProcessEnd                [BExpr]
  | ProcessYield              [BExpr]
  | ProcessAcquireChannel     [BExpr]
  | Acquire                   [BExpr]
  | ReleaseChannel            [BExpr]
  | ReleaseAllChannels        [BExpr]
  | ChannelRef                [BExpr]
  | ChannelIncrRefCount       [BExpr]
  | ChannelAcquireAndRegister [BExpr]
  | Awake                     [BExpr]
  | TryInputAction            [BExpr]
  | TryOutputAction           [BExpr]
  | RegisterInputCommitment   [BExpr]
  | RegisterOutputCommitment  [BExpr]
  | KnowRegister              [BExpr]
  | KnowSetForgetAll          [BExpr]
  | ReadyQueuePush            [BExpr]
  | InitIntValue              [BExpr]
  | InitStringValue           [BExpr]
  | InitBoolTrue              [BExpr]
  | InitBoolFalse             [BExpr]
  | BoolFromValue             [BExpr]
  | InitChannelValue          [BExpr]

data BExpr
  = Not BExpr
  | Equal BExpr BExpr
  | IntExpr Int
  | StringExpr String
  | Var VarName
  | Enum EnumName
  | FunCall RTFun
  | FunVal RTFun

type Lab = Int

data Instr
  = Comment String
  | Debug String
  | Seq Instr Instr
  | LexBloc Instr
  | ComBloc PilAST.Location Instr
  | Nop
  | DefFunction DefName ((VarName, Type), (VarName, Type)) Instr
  | EvalFunction EvalfuncName ((VarName, Type)) Instr
  | DeclareVar VarName Type
  | Assign VarName BExpr
  | Return
  | Goto Int
  | Increment VarName
  | Decrement VarName
  | If BExpr Instr Instr
  | Switch BExpr Instr
  | Case Int
  | CaseAndLabel Int
  | ProcCall RTFun

