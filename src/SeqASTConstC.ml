
open SeqAST


(* A version of the constant used in the backend conform to the C headers of libpirt *)

(* types *)

let pointer t = Pty ("*", t)

let void = Sty "void"
let pbool = Sty "bool"
let pint = Sty "int"

let pstring = pointer (Sty "char")

let pvalue = Sty "PICC_Value"
let sched_pool = Sty "PICC_SchedPool"

let pi_thread = Sty "PICC_PiThread"

let channel = Sty "PICC_Channel"
let mutex = Sty "PICC_Mutex"
let clock = Sty "PICC_Clock"


let commit = Sty "PICC_Commit"
(* in the C code the in_commit and out_commit are in fact just a field of the PICC_Commit structure*)
(* let in_commit = Sty "PICC_InCommit" *)
(* let out_commit = Sty "PICC_OutCommit" *)
let in_commit = commit
let out_commit = commit


let pc_label = Sty "PICC_Label"

let commit_list = Sty "PICC_CommitList"

let knows_set = Sty "PICC_KnownsSet"

let pset a = Pty ("set", a)

(* let queue a = Pty ("Queue", a) *)
let queue _ = Sty "PICC_Queue" (* PiThread queue *)
let ready_queue = Sty "PICC_ReadyQueue" 
let wait_queue = Sty "PICC_WaitQueue" 

(* let parray a = Pty ("array", a) *)

let eval_ty = Fun (pvalue, [pi_thread]) 

let pdef = Fun (void, [sched_pool; pi_thread])

(* enum types and their values *)

let status_enum = Sty "PICC_StatusKind"

let status_run = Val ("PICC_STATUS_RUN", status_enum)
let status_call = Val ("PICC_STATUS_CALL", status_enum)
let status_wait = Val ("PICC_STATUS_WAIT", status_enum)
let status_ended = Val ("PICC_STATUS_ENDED", status_enum)
let status_blocked = Val ("PICC_STATUS_BLOCKED", status_enum) (* cf compile guard choice + compile end*)


let try_result_enum = Sty "TryResultEnum"

let try_enabled = Val ("TryEnabled", try_result_enum)
let try_disabled = Val ("TryDisabled", try_result_enum)
let try_commit = Val ("TryCommit", try_result_enum)


let commit_status_enum = Sty "CommitStatusEnum"

let commit_cannot_acquire = Val ("CannotAcquire", commit_status_enum)
let commit_valid = Val ("ValidCommitment", commit_status_enum)
let commit_invalid = Val ("InvalidCommitment", commit_status_enum)

(* const values *)

let fuel_init = Val ("PICC_FUEL_INIT", pint)
let invalid_pc = Val ("InvalidPC", pc_label)

(* Runtime functions *)

let makeFun name ret args =
  SimpleName name, Fun (ret, args)

let awake = makeFun "PICC_awake" void [pointer sched_pool; pointer pi_thread(*; *error *)] 
let can_awake = makeFun "PICC_can_awake" commit_status_enum [pointer pi_thread; pointer commit(*; *error *)]
let channel_dec_ref_count = makeFun "PICC_channel_dec_ref_count" void [ pointer channel(*; *error *)]
let channel_incr_ref_count = makeFun "PICC_channel_incr_ref_count" void [ pointer channel(*; *error *)]

let fetch_input_commitment = makeFun "PICC_fetch_commitment" (pointer commit) [pointer channel (*; *error *)]
let fetch_output_commitment = fetch_input_commitment (* in the C code these functions are the same*)

let knows_register = makeFun "PICC_knowns_register" pbool [pointer knows_set; pointer channel(*; *error *)]
let knows_set_forget_all = makeFun "KnowsSetForgetAll" void [pointer knows_set] (*[MISSING]*)
let knows_set_forget_to_unknown = makeFun "KnowsSetForgetToUnknown" void [knows_set] (*[MISSING]*)

let knows_set_forget = makeFun "PICC_knowns_set_forget" 
(* (pset channel) *) (pointer knows_set) [pointer knows_set]

let knows_set_knows = makeFun "PICC_knowns_set_knows" 
(* (pset channel) *) (pointer knows_set) [pointer knows_set]

let register_input_commitment = makeFun "PICC_register_input_commitment" 
  void [pointer pi_thread; pointer channel; pint; pc_label]

let register_output_commitment = makeFun "PICC_register_output_commitment" 
  void [pointer pi_thread; pointer channel; pointer (Fun (pvalue, [pi_thread])); pc_label ]

let set_add = makeFun "PICC_set_add" pbool [pointer (pset channel); pointer channel] 
(*[MISSING] or [TO CORRECT]*)
let commit_list_is_empty = makeFun "IsEmpty" pbool [commit_list] (* [MISSING] *)

(* Thread Synchronization function *)
let wait_queue_push = makeFun "PICC_wait_queue_push" void [pointer wait_queue; pointer pi_thread]
let ready_queue_add = makeFun "PICC_ready_queue_add" void [pointer ready_queue; pointer pi_thread] 
let release_all_channels = makeFun "ReleaseAllChannels" void [pset channel] (* [MISSING] *)
let acquire = makeFun "PICC_acquire" void [pointer mutex]
let release = makeFun "PICC_release" void [pointer mutex]
let low_level_yield = makeFun "LowLevelYield" void [] (* [MISSING] *)

(* Misc *)
let emptySet = makeFun "PICC_set_make" (pointer (pset channel)) [] 
(*[TODO] do we use generic set or knowns_set in the later case, the fonction is PICC_create_knowns_set
  in the case of a generic set, i don't think the type should be passed as argument
*)

let p_inc v = Assign (v, (Op (Sum, Var v, Val ("1", pint))))
let p_dec v = Assign (v, (Op (Minus, Var v, Val ("1", pint))))

let return_void = Return (Val ("", void))


(*[TOCHECK] i think this two variable are always used as pointer*)

(* SchedPool fields *)
let scheduler = SimpleName "scheduler", pointer sched_pool
let sched_ready = (RecordName (scheduler, "ready"), (queue pi_thread)) (* ConcurrentReadyQueue?*)
let sched_wait = (RecordName (scheduler, "wait"), (queue pi_thread)) (* ConcurrentWaitQueue?*)

(* PiThread fields *)
let pt = (SimpleName "pt", pointer pi_thread)
let pt_status =(RecordName (pt, "status"), status_enum)
let pt_enabled i = (ArrayName ((RecordName (pt,"enabled") ), Val (string_of_int i, pint)), pbool)
let pt_knows = (RecordName (pt, "fuel"), knows_set)
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

(* NULL value *)
let null:value_t = "NULL", Sty "NULL"
