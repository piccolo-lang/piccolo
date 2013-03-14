
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
let pt_novalue = pointer (Sty "PICC_NoValue")

let channel = pointer (Sty "PICC_Channel")
let handle = pointer (Sty "PICC_Handle")

let sched_pool = pointer (Sty "PICC_SchedPool")
let pi_thread = pointer (Sty "PICC_PiThread")

let mutex = pointer (Sty "PICC_Mutex")
let clock = Sty "PICC_Clock"

let commit = pointer (Sty "PICC_Commit")
let in_commit = pointer (Sty "PICC_InCommit")
let out_commit = pointer (Sty "PICC_OutCommit")

let pc_label = Sty "PICC_Label"

let commit_list = Sty "PICC_CommitList"

let knownSet = pointer (Sty "PICC_KnownSet")
let knownValue = pointer (Sty "PICC_KnownValue")

let queue = Sty "PICC_Queue" 
let ready_queue = pointer (Sty "PICC_ReadyQueue")
let wait_queue = pointer (Sty "PICC_WaitQueue")

(* we need both eval_ty and eval_tyDef 
 * for the pretty printer to declare a function, we need a real function signature
 * to declare in the code a variable with the pointer type we need the second one
 * 
*)
let eval_ty = Fun (pt_value, [pi_thread]) 
let eval_tyDef = Sty "PICC_EvalFunction"

let eval_asvar = SimpleName "evalfunc", eval_tyDef


let pdef = Fun (void, [sched_pool; pi_thread]) (*PICC_PiThreadProc*)

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

let make_true   = SeqASTConstUtil.makeFun "PICC_create_bool_value" pt_bool [prim_bool]
let make_false  = SeqASTConstUtil.makeFun "PICC_create_bool_value" pt_bool [prim_bool]
let make_int    = SeqASTConstUtil.makeFun "PICC_create_int_value" pt_int [prim_int]
let make_string = SeqASTConstUtil.makeFun "PICC_create_string_value" pt_string [prim_string]

let make_list_n el n = 
  let rec f n acc = 
    if n = 0 then acc
    else f (n - 1) (el::acc)
  in f n []

let make_prim =
  fun module_name prim_name arity ->
    SeqASTConstUtil.makeFun (PrimitiveUtils.get_value_name module_name prim_name) pt_value (make_list_n pt_value arity) 

let create_bool = fun b -> CallFun (make_false, [Val (string_of_bool b, prim_bool)])
let create_int = fun n -> CallFun (make_int, [Val (string_of_int n, prim_int) ])
let create_string = fun str -> CallFun (make_string, [Val (str, prim_string) ])

let copy_value = "PICC_copy_value"

let bool_of_bool_value = "PICC_bool_of_bool_value"

let create_channel_value = "PICC_create_channel_value"

let outcommits_of_channel_value = "OUTCOMMITS_LIST_OF_VALUE"
let incommits_of_channel_value = "INCOMMITS_LIST_OF_VALUE"

let eval_fun_of_out_commit = "PICC_eval_func_of_output_commitment"

(* Runtime functions *)

let awake = "PICC_awake"
let can_awake = "PICC_can_awake"

let get_handle = "PICC_GET_HANDLE"
let acquire_handle = "PICC_ACQUIRE_HANDLE"
let handle_globalrc = "PICC_HANDLE_GLOBALRC"
let handle_dec_ref_count = "PICC_handle_dec_ref_count"
let handle_incr_ref_count = "PICC_handle_incr_ref_count"

let fetch_input_commitment = "PICC_FETCH_INPUT_COMMIT_FROM_VALUE"
let fetch_output_commitment = "PICC_FETCH_OUTPUT_COMMIT_FROM_VALUE"


let empty_knownSet = "PICC_create_empty_knownset"
let free_knownSet = "PICC_free_knownset"
let knownSet_add = "PICC_knownset_add"

let knownSet_register = "PICC_knownset_register"
let knownSet_forget_all = "PICC_knownset_forget_all"
let knownSet_forget_to_unknown = "PICC_knownset_forget_to_unknown"

let knownSet_forget = "PICC_knownset_forget"
let knownSet_known = "PICC_knownset_known"

let register_input_commitment = "PICC_REGISTER_INPUT_COMMITMENT_FROM_VALUE" 
let register_output_commitment = "PICC_REGISTER_OUTPUT_COMMITMENT_FROM_VALUE" 


let commit_list_is_empty = "PICC_commit_list_is_empty"

(* Thread Synchronization function *)
let wait_queue_push = "PICC_wait_queue_push"
let ready_queue_push = "PICC_ready_queue_push"
let ready_queue_add = "PICC_ready_queue_add"
let release_all_channels = "PICC_release_all_channels"

  (* channel = pointer so pointer channel is indeed an ** *)
let acquire = "PICC_acquire"
let release = "PICC_release"
let low_level_yield = "PICC_low_level_yield"


let generate_channel = "PICC_create_channel"
let generate_pi_thread = "PICC_create_pithread"



(* some key values *)
let null = "NULL", Sty "NULL"
let zero = "0", prim_int
let prim_false = "false", prim_bool
let prim_true = "true", prim_bool
let pc_label_init = "0", pc_label
let no_value = "PICC_create_no_value()", pt_novalue





(* SchedPool fields *)
let scheduler = "scheduler"
let sched_ready = "ready"
let sched_wait = "wait"

(* PiThread fields *)
let pt = "pt"
let pt_status = "status"
let pt_enabled = "enabled"
let pt_known = "knowns"
let pt_env = "env"
let pt_commit = "commit"
let pt_commits = "commits"
let pt_proc = "proc"
let pt_pc = "pc"
let pt_val = "val"
let pt_clock = "clock"
let pt_fuel = "fuel"

let pt_lock = "lock"
let pt_chans = "chans"



let try_result = SimpleName "tryresult", try_result_enum
let try_result_init = try_disabled
let nb_disabled_name = SimpleName "nbdisabled"

let ok_name = SimpleName "ok"
let vl_name = SimpleName "val"

let chan = SimpleName "chan", channel
let chans = SimpleName "chans", knownSet


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
let child_known = (RecordName (child, "knowns"), knownSet)
let child_env i = (ArrayName ((RecordName (child,"env") ), Val (string_of_int i, prim_int)), pt_value)

let tmp_val_name = SimpleName "tmp_val"

