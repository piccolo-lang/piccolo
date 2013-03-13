type varName =
  | SimpleName of string            (* name *)
  | RecordName of varDescr * string (* name.subField *)
  | ArrayName of varName * expr     (* name[i] or name.subField[i] *)
      
and piccType =
  | Sty of string (* Simple Type *)
  | Pty of string * piccType (* Parameterized type *)
  | Fun of piccType * ( piccType list ) 
      
and varDescr = varName * piccType
    
and value_t = string * piccType
    
and expr =
  | Val of value_t
  | Var of varDescr
  | Op of binop * expr * expr
  | CallFun of varDescr * (expr list)
      
and binop = 
  | Sum
  | Minus
  | Mult
  | Div
  | Equal
      
type instr =
  | Comment of string
  | Debug of string
  | Switch of expr * (instr list)
  | Case of expr
  | Bloc of instr list (* real bloc semantic *)
  | Seq of instr list (* just an instr list handler *)
  | CallProc of varDescr * (expr list) (* Call procedure *)
  | Declare of varDescr
  | Assign of varDescr * expr
  | DeclareFun of varDescr * (string list) * (instr list)
  | Foreach of varDescr * expr * (instr list) (* foreach (name : type) in (Fun) do () *)
  | Ite of expr * (instr list) * (instr list)
  | Label of string
  | Goto of string
  | Return of expr
  | DoWhile of (instr list) * expr
      
module type Consts =
sig
  val void : piccType
    (* primitive int the sense that it's a primitive type of the target language
     * for instance in C it's just the plain int
     *)
    
  val prim_bool : piccType
  val prim_int : piccType
  val prim_string : piccType
    
  (* représentation of the types in the runtime library *)
    
  val pt_value : piccType
  val pt_bool : piccType
  val pt_int : piccType
  val pt_string : piccType
  val pt_channel : piccType
  val pt_novalue : piccType

  val channel : piccType
    
  val sched_pool : piccType
  val pi_thread : piccType
    
  val mutex : piccType
  val clock : piccType
    
  val commit : piccType
  val in_commit : piccType
  val out_commit : piccType
    
  val pc_label : piccType
  val commit_list : piccType
    
  val knows_set : piccType
    
  val pset : piccType -> piccType
    
  val queue : piccType -> piccType
    
  val pdef : piccType

  val eval_ty : piccType
    
  val eval_asvar : varDescr
    (* enum types and their values *)
    (* the values are given as expr since they're only use in assignenment *)
  val status_enum : piccType
    
  val status_run : expr
  val status_call : expr
  val status_wait : expr
  val status_ended : expr
  val status_blocked : expr
    
  val try_result_enum : piccType
    
  val try_enabled : expr
  val try_disabled : expr
  val try_commit : expr
    
  val commit_status_enum: piccType
    
  val commit_cannot_acquire: expr
  val commit_valid: expr
  val commit_invalid: expr
    
  (* const values *)
    
  val fuel_init: expr
  val invalid_pc: expr
    
  (* value creation *)
  val create_bool: bool -> expr
  val create_int: int -> expr
  val create_string : string -> expr
  (* val create_tuple *)
    
  val make_prim: string -> string -> int -> varDescr
    
  (* one idea would be to refactor the code so that in SeqASTConst_.ml
     we declare only varName and the corresponding varDescr is defined 
     in Backend.ml : it would ensure the typing
  *)    
  val copy_value: varDescr
  val bool_of_boolval: varDescr
    
  val pt_channel_of_channel: varDescr
  val channel_of_pt_channel: varDescr
  val acquire_channel: varDescr
  val channel_globalrc: varDescr
  val eval_fun_of_out_commit: varDescr
    
  (* Runtime functions *)
  val awake : varDescr
  val can_awake : varDescr
  val channel_dec_ref_count : varDescr
  val channel_incr_ref_count : varDescr
    
  val fetch_input_commitment : varDescr
  val fetch_output_commitment : varDescr
    
  val knows_register : varDescr
  val knows_set_forget_all : varDescr
  val knows_set_forget_to_unknown : varDescr
    
  val knows_set_forget : varDescr
  val knows_set_knows : varDescr
    
  val register_input_commitment : varDescr
    
  val register_output_commitment : varDescr
  val set_add : varDescr
  val commit_list_is_empty : varDescr
    
  (* Thread Synchronization function *)
  val wait_queue_push : varDescr
  val ready_queue_push : varDescr
  val ready_queue_add : varDescr
  val release_all_channels : varDescr
  val acquire : varDescr
  val release : varDescr
  val low_level_yield : varDescr
    
  val generate_channel : varDescr
  val generate_pi_thread : varDescr
    
  (* Misc *)
  val emptySet : varDescr
  val emptyKnownSet : varDescr
  val freeKnownSet : varDescr
    
  val p_inc : varDescr -> instr 
  val p_dec : varDescr -> instr
    
  val return_void : instr
    
  (* SchedPool fields *)
  val scheduler : varDescr
  val sched_ready : varDescr
  val sched_wait : varDescr
    
  (* PiThread fields *)
  val pt : varDescr
  val pt_status : varDescr
  val pt_enabled : int -> varDescr
  val pt_knows : varDescr
  val pt_env : int -> varDescr
  val pt_commit : varDescr
  val pt_commits : varDescr
  val pt_proc : varDescr
  val pt_pc : varDescr
  val pt_val : varDescr
  val pt_clock : varDescr
  val pt_fuel : varDescr
  val pt_lock : varDescr
  val pt_chans : varDescr  
    
  val try_result : varDescr
  val try_result_init : expr
  val nb_disabled_name : varName
  val ok_name : varName
  val vl_name : varName

  val chan : varDescr (* tmp var used in foreach loops *) 
  val chans : varDescr (* channel set*)
  val tmp_chan_name : varName
  
  val in_chan_name : varName
  val outcommits_field : string
  val in_chanx_name : varName

  val out_chan_name : varName
  val incommits_field : string
  val newchan_name : varName

  val d_entry : expr (* value of the definition entry point*)

  val ocommit_var : varDescr
  val ocommit_thread : varDescr
    
  val icommit_var : varDescr
  val icommit_thread : varDescr
  val icommit_refvar : varDescr
  val icommit_thread_env_rv : varDescr
    

  val args : int -> varDescr
  val child : varDescr

  val child_proc : varDescr
  val child_pc : varDescr
  val child_status : varDescr
  val child_knows : varDescr
  val child_env : int -> varDescr  

  val tmp_val_name : varName
    
  (* some key values *)
  val null:value_t
  val zero: value_t
  val prim_false: value_t
  val pc_label_init: value_t
  val no_value: value_t
end
  
module type PrettyPrinter =
sig
  
  val string_name_of_varDescr : varDescr -> string

  val print_piccType : Format.formatter -> piccType -> unit
    
  val print_binop : Format.formatter -> binop -> unit
    
  val print_varName : Format.formatter -> varName -> unit
    
  val print_expr : Format.formatter -> expr -> unit
    
  val print_instr : Format.formatter -> instr -> unit
    
  val print_instr_list_std : instr list -> unit
    
  val print_main : int -> string -> int -> int -> int -> Format.formatter -> instr -> unit
end
