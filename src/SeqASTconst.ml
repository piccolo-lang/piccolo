
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

let knows_set = Sty "KnowsSet"
let pset a = Pty ("set", a)

let queue a = Pty ("Queue", a)

let eval_ty = Fun (pvalue, [pi_thread]) 

(* enum types and their values *)

let status_enum = Sty "StatusEnum"

let status_run = Val ("StatusRun", status_enum)
let status_call = Val ("StatusCall", status_enum)
let status_wait = Val ("StatusWait", status_enum)
let status_ended = Val ("StatusEnded", status_enum)
let status_blocked = Val ("StatusBlocked", status_enum) (* cf compile guard choice + compile end*)

let try_result_enum = Sty "TryResultEnum"
 
let try_enabled = Val ("TryEnabled", try_result_enum)
let try_disabled = Val ("TryEnabled", try_result_enum)
let try_commit = Val ("TryEnabled", try_result_enum)


(* const values *)

let fuel_init = Val ("FuelInit", pint)
let invalid_pc = Val ("InvalidPC", pc_label)

(* Runtime functions *)

let makeFun name ret args =
  let name = SimpleName name in
    name, Fun (ret, args)

let awake = makeFun "Awake" void [sched_pool; pi_thread]
let can_awake = makeFun "CanAwake" pbool [pi_thread; commit]
let channel_dec_ref_count = makeFun "ChannelDecRefCount" void [ channel ]
let channel_incr_ref_count = makeFun "ChannelIncrRefCount" void [ channel ]
let fetch_input_commitment = makeFun "FetchInputCommitment" in_commit [pi_thread; channel]
let fetch_output_commitment = makeFun "FetchOutputCommitment" out_commit [pi_thread; channel] 

let knows_register = makeFun "KnowsRegister" pbool [knows_set; channel]
let knows_set_forget_all = makeFun "KnowsSetForgetAll" void [knows_set]

let knows_set_forget_to_unknown = makeFun "KnowsSetForgetToUnknown" void [knows_set]
let knows_set_forget = makeFun "KnowsSetForget" (pset channel) [knows_set]
let knows_set_knows = makeFun "KnowsSetKnows" (pset channel) [knows_set]

let register_input_commitment = makeFun "RegisterInputCommitment" void [pi_thread; channel; pint; pc_label]
let register_output_commitment = makeFun "RegisterOutputCommitment" void [pi_thread; channel; Fun (pvalue, [pi_thread]); pc_label ]
let setAdd = makeFun "SetAdd"

(* Thread Synchronization function *)
let wait_queue_push = makeFun "WaitQueuePush" void [queue pi_thread; pi_thread] (* WaitQueue *)
let ready_queue_add = makeFun "ReadyQueueAdd" void [queue pi_thread; pi_thread] (* WaitQueue *)
let release_all_channels = makeFun "ReleaseAllChannels" void [pset channel]
let acquire = makeFun "Acquire" void [mutex]
let release = makeFun "Release" void [mutex]
let low_level_yield = makeFun "LowLevelYield" void []

(* Misc *)
let emptySet = makeFun "EmptySet" (pset channel) []

let p_inc v = Assign (v, (Op (Sum, Var v, Val ("1", pint))))
let p_dec v = Assign (v, (Op (Minus, Var v, Val ("1", pint))))

let return_void = Return (Val ("null", void))

(* SchedPool fields *)

let sched = SimpleName "scheduler"
let sched_ready = (RecordName (sched, "ready"), (queue pi_thread)) (* ConcurrentReadyQueue?*)
let sched_wait = (RecordName (sched, "wait"), (queue pi_thread)) (* ConcurrentWaitQueue?*)

(* PiThread fields *)
let pt_name = SimpleName "pt"
let pt = (pt_name, pi_thread)
let pt_status =(RecordName (pt_name, "status"), status_enum)
let pt_enabled i = (ArrayName ((RecordName (pt_name,"enabled") ), i), pbool)
let pt_knows = (RecordName (pt_name, "fuel"), knows_set)
let pt_env i = (ArrayName ((RecordName (pt_name,"env") ), i), pvalue)
let pt_commit = (RecordName (pt_name, "commit"), commit)
let pt_commits = (RecordName (pt_name, "commits"), (pset commit))
(* let pt_proc = (RecordName (pt_name, "proc"), !! function trouver signature!!) *)
let pt_pc = (RecordName (pt_name, "pc"), pc_label)
let pt_val = (RecordName (pt_name, "val"), pvalue)
let pt_clock = (RecordName (pt_name, "clock"), clock)
let pt_fuel = (RecordName (pt_name, "fuel"), pint)
let pt_lock = (RecordName (pt_name, "lock"), mutex)
