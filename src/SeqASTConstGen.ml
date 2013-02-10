
open SeqAST


(* types *)

let void = Sty "void"
let pbool = Sty "bool"
let pint = Sty "int"
let pstring = Sty "string"

let pvalue = Sty "value"
let sched_pool = Sty "SchedPool"
let pi_thread = Sty "PiThread"
let channel = Sty "Channel"
let mutex = Sty "Mutex"
let clock = Sty "Clock"
let commit = Sty "Commit"
let in_commit = Sty "InCommit"
let out_commit = Sty "OutCommit"
let pc_label = Sty "Label"

let commit_list = Sty "CommitList"

let knows_set = Sty "KnowsSet"

let pset a = Pty ("set", a)
let queue a = Pty ("Queue", a)

(* let parray a = Pty ("array", a) *)

let eval_ty = Fun (pvalue, [pi_thread]) 

let pdef = Fun (void, [sched_pool; pi_thread])

(* enum types and their values *)

let status_enum = Sty "StatusEnum"

let status_run = Val ("StatusRun", status_enum)
let status_call = Val ("StatusCall", status_enum)
let status_wait = Val ("StatusWait", status_enum)
let status_ended = Val ("StatusEnded", status_enum)
let status_blocked = Val ("StatusBlocked", status_enum) (* cf compile guard choice + compile end*)

let try_result_enum = Sty "TryResultEnum"
 
let try_enabled = Val ("TryEnabled", try_result_enum)
let try_disabled = Val ("TryDisabled", try_result_enum)
let try_commit = Val ("TryCommit", try_result_enum)


let commit_status_enum = Sty "CommitStatusEnum"

let commit_cannot_acquire = Val ("CannotAcquire", commit_status_enum)
let commit_valid = Val ("ValidCommitment", commit_status_enum)
let commit_invalid = Val ("InvalidCommitment", commit_status_enum)

(* const values *)

let fuel_init = Val ("FuelInit", pint)
let invalid_pc = Val ("InvalidPC", pc_label)

(* Runtime functions *)

let makeFun name ret args =
  SimpleName name, Fun (ret, args)

let awake = makeFun "Awake" void [sched_pool; pi_thread]
let can_awake = makeFun "CanAwake" commit_status_enum [pi_thread; commit]
let channel_dec_ref_count = makeFun "ChannelDecRefCount" void [ channel ]
let channel_incr_ref_count = makeFun "ChannelIncrRefCount" void [ channel ]
let fetch_input_commitment = makeFun "FetchInputCommitment" in_commit [channel]
let fetch_output_commitment = makeFun "FetchOutputCommitment" out_commit [channel] 

let knows_register = makeFun "KnowsRegister" pbool [knows_set; channel]
let knows_set_forget_all = makeFun "KnowsSetForgetAll" void [knows_set]

let knows_set_forget_to_unknown = makeFun "KnowsSetForgetToUnknown" void [knows_set]
let knows_set_forget = makeFun "KnowsSetForget" (pset channel) [knows_set]
let knows_set_knows = makeFun "KnowsSetKnows" (pset channel) [knows_set]

let register_input_commitment = makeFun "RegisterInputCommitment" void [pi_thread; channel; pint; pc_label]
let register_output_commitment = makeFun "RegisterOutputCommitment" void [pi_thread; channel; Fun (pvalue, [pi_thread]); pc_label ]

let set_add = makeFun "SetAdd" pbool [pset channel; channel]
(* [TODO] voir avec maxence, il semblerait que ses add renvoit void - besoin bool cf compileTryAction *)

let commit_list_is_empty = makeFun "IsEmpty" pbool [commit_list]

(* Thread Synchronization function *)
let wait_queue_push = makeFun "WaitQueuePush" void [queue pi_thread; pi_thread] (* WaitQueue *)
let ready_queue_push = makeFun "ReadyQueuePush" void [queue pi_thread; pi_thread]
let ready_queue_add = makeFun "ReadyQueueAdd" void [queue pi_thread; pi_thread] (* WaitQueue *)
let release_all_channels = makeFun "ReleaseAllChannels" void [pset channel]
let acquire = makeFun "PIT_acquire" void [mutex]
let release = makeFun "Release" void [mutex]
let low_level_yield = makeFun "LowLevelYield" void []

let generate_channel = makeFun "GenerateChannel" channel [] 
let generate_pi_thread = makeFun "GeneratePiThread" pi_thread [] 

(* Misc *)
let emptySet = makeFun "EmptySet" (pset channel) []

let p_inc v = Assign (v, (Op (Sum, Var v, Val ("1", pint))))
let p_dec v = Assign (v, (Op (Minus, Var v, Val ("1", pint))))

let return_void = Return (Val ("null", void))

(* SchedPool fields *)
let scheduler = SimpleName "scheduler", sched_pool
let sched_ready = (RecordName (scheduler, "ready"), (queue pi_thread)) (* ConcurrentReadyQueue?*)
let sched_wait = (RecordName (scheduler, "wait"), (queue pi_thread)) (* ConcurrentWaitQueue?*)

(* PiThread fields *)
let pt = (SimpleName "pt", pi_thread)
let pt_status =(RecordName (pt, "status"), status_enum)
let pt_enabled i = (ArrayName ((RecordName (pt,"enabled") ), Val (string_of_int i, pint)), pbool)
let pt_knows = (RecordName (pt, "knows"), knows_set)
let pt_env i = (ArrayName ((RecordName (pt,"env") ), Val (string_of_int i, pint)), pvalue)
let pt_env_lock i = (RecordName (pt_env i ,"lock") , mutex)
let pt_commit = (RecordName (pt, "commit"), commit)
let pt_commits = (RecordName (pt, "commits"), (pset commit))
let pt_proc = (RecordName (pt, "proc"), pdef)
let pt_pc = (RecordName (pt, "pc"), pc_label)
let pt_val = (RecordName (pt, "val"), pvalue)
let pt_clock = (RecordName (pt, "clock"), clock)
let pt_fuel = (RecordName (pt, "fuel"), pint)
let pt_lock = (RecordName (pt, "lock"), mutex)


let try_result = SimpleName "tryresult", try_result_enum
let chan = SimpleName "chan", channel
  

(* NULL value *)
let null:value_t = "NULL", Sty "NULL"
