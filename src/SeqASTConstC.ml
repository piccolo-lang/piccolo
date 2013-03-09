
open SeqAST


(* A version of the constant used in the backend conform to the C headers of libpirt *)
(* types *)

let pointer t = Pty ("*", t)

let void = Sty "void"

let prim_bool = Sty "bool"
let prim_int = Sty "int"
let prim_string = pointer (Sty "char")


let pt_value = pointer (Sty "PICC_Value")

let pt_bool = pointer (Sty "PICC_BoolValue")
let pt_int = pointer (Sty "PICC_IntValue")
let pt_string = pointer (Sty "PICC_StringValue")
let pt_channel = pointer (Sty "PICC_ChannelValue")

let channel = pointer (Sty "PICC_Channel")

let sched_pool = Sty "PICC_SchedPool"
let pi_thread = pointer (Sty "PICC_PiThread")

let mutex = Sty "PICC_Mutex"
let clock = Sty "PICC_Clock"

let commit = pointer (Sty "PICC_Commit")
(* in the C code the in_commit and out_commit are in fact just a field of the PICC_Commit structure*)
let in_commit = pointer (Sty "PICC_InCommit")
let out_commit = pointer (Sty "PICC_OutCommit")

let pc_label = Sty "PICC_Label"

let commit_list = Sty "PICC_CommitList"

let knows_set = pointer (Sty "PICC_KnownSet")
(* typedef struct _KnownSet* PICC_KnownSet; -> knownset already a pointer *)


(* the knows_set is implemented as a generic set*)
let pset _ = knows_set
(* let pset a = Pty ("", a) *)

(* let queue a = Pty ("Queue", a) *)
let queue _ = Sty "PICC_Queue" (* PiThread queue *)
let ready_queue = Sty "PICC_ReadyQueue" 
let wait_queue = Sty "PICC_WaitQueue" 

(* let parray a = Pty ("array", a) *)

(* we need both eval_ty and eval_tyDef 
 * for the pretty printer to declare a function, we need a real function signature
 * to declare in the code a variable with the pointer type we need the second one
 * 
*)
let eval_ty = Fun (pt_value, [pi_thread]) 
let eval_tyDef = Sty "PICC_EvalFunction"

let eval_asvar = SimpleName "evalfunc", eval_tyDef


let pdef = Fun (void, [pointer sched_pool; pi_thread]) (*PICC_PiThreadProc*)

(* enum types and their values *)

let status_enum = Sty "PICC_StatusKind"

let status_run = Val ("PICC_STATUS_RUN", status_enum)
let status_call = Val ("PICC_STATUS_CALL", status_enum)
let status_wait = Val ("PICC_STATUS_WAIT", status_enum)
let status_ended = Val ("PICC_STATUS_ENDED", status_enum)
let status_blocked = Val ("PICC_STATUS_BLOCKED", status_enum) (* cf compile guard choice + compile end*)


let try_result_enum = Sty "PICC_TryResult"

let try_enabled = Val ("PICC_TRY_ENABLED", try_result_enum)
let try_disabled = Val ("PICC_TRY_DISABLED", try_result_enum)
let try_commit = Val ("PICC_TRY_COMMIT", try_result_enum)


let commit_status_enum = Sty "PICC_CommitStatus"

let commit_cannot_acquire = Val ("PICC_CANNOT_ACQUIRE", commit_status_enum)
let commit_valid = Val ("PICC_VALID_COMMIT", commit_status_enum)
let commit_invalid = Val ("PICC_INVALID_COMMIT", commit_status_enum)

(* const values *)

let fuel_init = Val ("PICC_FUEL_INIT", prim_int)
let invalid_pc = Val ("PICC_INVALID_PC", pc_label)

(* value creation *)

let makeFun name ret args =
  SimpleName name, Fun (ret, args)

let make_true   = makeFun "PICC_create_bool_value" pt_bool [prim_bool]
let make_false  = makeFun "PICC_create_bool_value" pt_bool [prim_bool]
let make_int    = makeFun "PICC_create_int_value" pt_int [prim_int]
let make_string = makeFun "PICC_create_string_value" pt_string [prim_string]

let make_list_n el n = 
  let rec f n acc = 
    if n = 0 then acc
    else f (n - 1) (el::acc)
  in f n []

let make_prim =
  fun module_name prim_name arity ->
    makeFun (Prim.get_value_name module_name prim_name) pt_value (make_list_n pt_value arity) 

let create_bool = fun b -> CallFun (make_false, [Val (string_of_bool b, prim_bool)])
let create_int = fun n -> CallFun (make_int, [Val (string_of_int n, prim_int) ])
let create_string = fun str -> CallFun (make_string, [Val (str, prim_string) ])

let copy_value = makeFun "PICC_copy_value" prim_bool [pt_value; pt_value]

let bool_of_boolval = makeFun "PICC_bool_of_bool_value" prim_bool [pt_value]

let pt_channel_of_channel = makeFun "PICC_create_channel_value" pt_channel [channel]
let channel_of_pt_channel = makeFun "PICC_channel_of_channel_value" channel [pt_value]

let acquire_channel = makeFun "PICC_channel_value_acquire" void [pt_value]
let channel_globalrc = makeFun "PICC_channel_value_global_rc" prim_int [pt_value]

let eval_fun_of_out_commit = makeFun "PICC_eval_func_of_output_commitment" eval_ty [commit]

(* Runtime functions *)

let awake = makeFun "PICC_awake" void [pointer sched_pool; pi_thread; commit] 
let can_awake = makeFun "PICC_can_awake" commit_status_enum [pi_thread; commit]
let channel_dec_ref_count = makeFun "PICC_channel_dec_ref_count" void [ channel]
let channel_incr_ref_count = makeFun "PICC_channel_incr_ref_count" void [ channel]

let fetch_input_commitment = makeFun "PICC_fetch_input_commitment" commit [channel]
let fetch_output_commitment = makeFun "PICC_fetch_output_commitment" commit [channel]

let knows_register = makeFun "PICC_knowns_register" pt_bool [knows_set; channel]
let knows_set_forget_all = makeFun "PICC_knowns_set_forget_all" void [knows_set]
let knows_set_forget_to_unknown = makeFun "PICC_knowns_set_forget_to_unknown" void [knows_set; channel]

let knows_set_forget = makeFun "PICC_knowns_set_forget" knows_set [knows_set]
let knows_set_knows = makeFun "PICC_knowns_set_knows" knows_set [knows_set]

let register_input_commitment = makeFun "PICC_register_input_commitment" 
  void [pi_thread; channel; prim_int; pc_label]

let register_output_commitment = makeFun "PICC_register_output_commitment" 
  void [pi_thread; channel; pointer (Fun (pt_value, [pi_thread])); pc_label ]

let set_add = makeFun "PICC_known_set_add_channel" prim_bool [knows_set; channel] 
let commit_list_is_empty = makeFun "PICC_commit_list_is_empty" pt_bool [commit_list]

(* Thread Synchronization function *)
let wait_queue_push = makeFun "PICC_wait_queue_push" void [pointer wait_queue; pi_thread]
let ready_queue_push = makeFun "PICC_ready_queue_push" void [queue pi_thread; pi_thread]
let ready_queue_add = makeFun "PICC_ready_queue_add" void [pointer ready_queue; pi_thread] 
let release_all_channels = makeFun "PICC_release_all_channels" void [pointer channel]

  (* channel = pointer so pointer channel is indeed an ** *)
let acquire = makeFun "PICC_acquire" void [pointer mutex]
let release = makeFun "PICC_release" void [pointer mutex]
let low_level_yield = makeFun "PICC_low_level_yield" void []


let generate_channel = makeFun "PICC_create_channel" channel []
let generate_pi_thread = makeFun "PICC_create_pithread" pi_thread [prim_int; prim_int; prim_int] 
(*env_length, knows_length, nb_enabled*) 


(* Misc *)
let emptySet = makeFun "PICC_CHANNEL_SET_MAKE" (pointer (pset channel)) [] 

(* some key values *)
let null = "NULL", Sty "NULL"
let zero = "0", prim_int
let prim_false = "false", prim_bool
let prim_true = "true", prim_bool
let pc_label_init = "0", pc_label

(* Variables *)

(* we find here the description of all the variables used to generate code in the backend. 
   (except the one that have a runtime generated name of course)
 *)

(* SchedPool fields *)
let scheduler = SimpleName "scheduler", pointer sched_pool
let sched_ready = (RecordName (scheduler, "ready"), (queue pi_thread)) (* ConcurrentReadyQueue?*)
let sched_wait = (RecordName (scheduler, "wait"), (queue pi_thread)) (* ConcurrentWaitQueue?*)

(* PiThread fields *)
let pt = (SimpleName "pt", pi_thread)
let pt_status =(RecordName (pt, "status"), status_enum)
let pt_enabled i = (ArrayName ((RecordName (pt,"enabled") ), Val (string_of_int i, prim_int)), pt_bool)
let pt_knows = (RecordName (pt, "knowns"), knows_set)
let pt_env i = (ArrayName ((RecordName (pt,"env") ), Val (string_of_int i, prim_int)), pt_value)
let pt_commit = (RecordName (pt, "commit"), commit)
let pt_commits = (RecordName (pt, "commits"), (pset commit))
let pt_proc = (RecordName (pt, "proc"), pdef)
let pt_pc = (RecordName (pt, "pc"), pc_label)
let pt_val = (RecordName (pt, "val"), pt_value)
let pt_clock = (RecordName (pt, "clock"), clock)
let pt_fuel = (RecordName (pt, "fuel"), prim_int)

let pt_lock = (RecordName (pt, "lock"), mutex)


let try_result = SimpleName "tryresult", try_result_enum
let nb_disabled_name = SimpleName "nbdisabled"

let ok_name = SimpleName "ok"
let vl_name = SimpleName "val"


let chan = SimpleName "chan", channel
let chans = SimpleName "chans", (pset channel)
let chans_init_value = null
let tmp_chan_name = SimpleName "tmp_chan"

let in_chan_name = SimpleName "in_chan"
let outcommits_field = "outcommits"
let in_chanx_name = SimpleName "in_chanx"

let out_chan_name = SimpleName "out_chan"
let incommits_field = "incommits"
let newchan_name = SimpleName "newchan"


let d_entry = Val ("0", prim_int)

let ocommit_var = SimpleName "commit", commit
let ocommit_thread = RecordName (ocommit_var, "thread"), pi_thread
  
let icommit_var = SimpleName "commit", commit 
let icommit_thread = RecordName (icommit_var, "thread"), pi_thread 
let icommit_in = RecordName (icommit_var, "content.in"), in_commit
let icommit_refvar = RecordName (icommit_in, "refvar"), prim_int 
let icommit_thread_env_rv = 
  ArrayName (RecordName (icommit_thread, "env"), Var icommit_refvar), pt_value 


let args i= (ArrayName (SimpleName "args", Val (string_of_int i, prim_int)), pt_value)
let child = SimpleName "child", pi_thread

let child_proc = (RecordName (child, "proc"), pdef)
let child_pc = (RecordName (child, "pc"), pc_label)
let child_status =(RecordName (child, "status"), status_enum)
let child_knows = (RecordName (child, "knowns"), knows_set)
let child_env i = (ArrayName ((RecordName (child,"env") ), Val (string_of_int i, prim_int)), pt_value)


(* Utils *)
let p_inc v = Assign (v, (Op (Sum, Var v, Val ("1", prim_int))))
let p_dec v = Assign (v, (Op (Minus, Var v, Val ("1", prim_int))))

let return_void = Return (Val ("", void))

