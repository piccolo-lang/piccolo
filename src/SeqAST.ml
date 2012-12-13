
type label = 
| DefLabel of int
| ContLabel of string

type varName =
| SimpleName of string (* name *)
| RecordName of varName * varName (* name.subField *)
| ArrayName  of varName * int (* name[i] or name.subField[i] *)
| FunName of varName * varName * (varName list) (* cf commit.evalfunc(commit.thread) p17 *)

(* pour le affectations *)
type ptStatus =
| StatusRun
| StatusCall
| StatusWait
| StatusEnded
 
type tryResultEnum =
| Enabled
| Disabled
| Commit

(* Pour les déclarations *)
type piccType =
| Bool
| Int
| Value
| SchedPool
| PiThread
| Channel
| InCommit
| OutCommit


type runTimeFunction =
| Awake of varName * varName * varName
| CanAwake of varName * varName
| ChannelDecRefCount of varName
| ChannelIncrRefCount of varName
| FetchInputCommitment of varName
| FetchOutputCommitment of varName
| KnowsRegister of varName * varName
| KnowsSetForget of varName
| KnowsSetForgetAll of varName
| KnowsSetForgetToUnknown of varName * varName
| KnowsSetKnows of varName
| RegisterInputCommitment of varName * varName * varName * varName
| RegisterOutputCommitment of varName * varName * varName * varName
| SetAdd of varName * varName

(* Thread Synchronization function *)
| WaitQueuePush of varName * varName
| Release of varName
| ReadyQueueAdd of varName * varName
| ReleaseAllChannels of varName
| Acquire of varName
| ReleaseAllChannels of varName
| LowLevelYield


type instr =
| Seq of instr list
| Call of runTimeFunction
| Declaration of varName * piccType (* name : type *)
| Assignment of varName (* * value *)
| Foreach of varName * piccType * runTimeFunction * instr (* foreach name : type in Fun() *)
