
open SeqAST

(* A version of the constant used in the backend conform to the C headers of libpirt *)
(* types *)

let pointer t = Pty ("*", t)

let void = Sty "void"

let prim_bool = Sty "bool"
let prim_int = Sty "int"
let prim_string = pointer (Sty "char")

let pt_value = Sty "PICC_Value"

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

let init_bool_true = "PICC_INIT_BOOL_TRUE"
let init_bool_false = "PICC_INIT_BOOL_FALSE"
let init_int_value = "PICC_INIT_INT_VALUE"
let init_string_value = "PICC_INIT_STRING_VALUE"
let init_channel_value = "PICC_INIT_CHANNEL_VALUE"

let make_true   = SeqASTConstUtil.makeFun init_bool_true void [pt_value]
let make_false  = SeqASTConstUtil.makeFun init_bool_false void [pt_value]
let make_int    = SeqASTConstUtil.makeFun init_int_value void [pt_value; prim_int]
let make_string = SeqASTConstUtil.makeFun init_string_value void [pt_value; prim_string]
let make_channel= SeqASTConstUtil.makeFun init_channel_value void [pt_value; channel]

let make_list_n el n = 
  let rec f n acc = 
    if n = 0 then acc
    else f (n - 1) (el::acc)
  in f n []

let make_string_handle = SeqASTConstUtil.makeFun "PICC_create_string_handle" handle [prim_string]
let create_string_handle = fun str -> CallFun (make_string_handle, [Val (str, prim_string) ])

let copy_value = "PICC_copy_value"

let bool_of_bool_value = "PICC_BOOL_OF_BOOL_VALUE"

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
(* let no_value = "PICC_create_no_value()", pt_novalue *)


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

let try_result = "tryresult"
let try_result_init = try_disabled

let nb_disabled_name = "nbdisabled"

let ok_name = "ok"
let vl_name = "val"

let chan = "chan"
let chans = "chans"

let d_entry = Val ("0", pc_label)


let ocommit_var = "commit"
let ocommit_thread = "thread"
let ocommit_thread_val = "val"
  
let icommit_var = "commit"
let icommit_thread = "thread"
let icommit_in = "content.in"
let icommit_refvar = "refvar"
let icommit_thread_env_rv = "env"


let args = "args"
let child = "child"
let child_proc = "proc"
let child_pc = "pc"
let child_status = "status"
let child_known = "knowns"
let child_env = "env"

(* Primitives *)
let add_name = "PICC_Int_add";;
let substract_name = "PICC_Int_substract";;
let modulo_name = "PICC_Int_modulo";;
let equals_name = "PICC_equals";;
let less_than_name = "PICC_Int_less_than";;
let print_info_name = "PICC_print_value_infos";;
let print_str_name = "PICC_print_value";;
let print_int_name = "PICC_print_value";;
