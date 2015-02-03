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
  | PrimCall PrimName           [BExpr]
  | DefProc  DefName

  -- pithread.h
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
  | RegisterRegisterValue       BExpr
  | ProcessLock                 BExpr
  | ProcessLockChannel          BExpr BExpr
  | ProcessYield                BExpr BExpr
  | ProcessWait                 BExpr BExpr
  | ProcessAwake                BExpr BExpr
  | ProcessEnd                  BExpr BExpr

  -- value.h
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

  -- scheduler.h
  | ReadyPushFront              BExpr BExpr
  | SchedGetReadyQueue          BExpr

  -- commit.h
  | GetThread                   BExpr
  | GetRefVar                   BExpr
  | CallEvalFunc                BExpr
  | RegisterInputCommitment     BExpr BExpr BExpr BExpr
  | RegisterOutputCommitment    BExpr BExpr BExpr BExpr

  -- tryaction.h
  | TryInputAction              BExpr BExpr
  | TryOutputAction             BExpr BExpr
  | ChannelArrayCreate          BExpr
  | ChannelArrayLockAndRegister BExpr BExpr BExpr BExpr
  | ChannelArrayUnlock          BExpr BExpr


data BExpr
  = Not BExpr
  | Equal BExpr BExpr
  | BoolExpr Bool
  | IntExpr Int
  | FloatExpr Float
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
  | ReturnRegister
  | Goto Int
  | Increment VarName
  | Decrement VarName
  | If BExpr Instr Instr
  | Switch BExpr Instr
  | Case Int
  | CaseAndLabel Int
  | ProcCall RTFun
