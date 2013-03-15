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


module type Prims =
sig
  val add_name : string
  val substract_name : string
  val compare_name : string
  val print_info_name : string
  val print_str_name : string
  val print_int_name : string
end

module type OutputTypes =
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
  val handle : piccType

  val sched_pool : piccType
  val pi_thread : piccType
    
  val mutex : piccType
  val clock : piccType
    
  val commit : piccType
  val in_commit : piccType
  val out_commit : piccType
    
  val pc_label : piccType
  val commit_list : piccType
    
  val knownSet : piccType
  val knownValue : piccType

  val queue : piccType
  val ready_queue : piccType
  val wait_queue : piccType
    
  val pdef : piccType

  val eval_ty : piccType
   
  val eval_asvar : varDescr

  (* enum types and their values *)
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
    
    
  val try_result : varDescr
  val try_result_init : expr
  val nb_disabled_name : varName
  val ok_name : varName
  val vl_name : varName

  val chan : varDescr (* tmp var used in foreach loops *) 
  val chans : varDescr (* channel set*)
  
  val d_entry : expr (* value of the definition entry point*)

  val ocommit_var : varDescr
  val ocommit_thread : varDescr
    
  val icommit_var : varDescr
  val icommit_thread : varDescr
  val icommit_refvar : varDescr
  val icommit_thread_env_rv : varDescr
    
  val args : int -> varDescr
  val arg_init_value : value_t
  val child : varDescr

  val child_proc : varDescr
  val child_pc : varDescr
  val child_status : varDescr
  val child_known : varDescr
  val child_env : int -> varDescr  
    
  (* some key values *)
  val null:value_t
  val zero: value_t
  val prim_false: value_t
  val pc_label_init: value_t
  val no_value: value_t

end

(**************************************)
(**************************************)
(**************************************)
  

module type Names =
sig
  val copy_value: string
  val bool_of_bool_value: string
    
  val create_channel_value: string

  val outcommits_of_channel_value : string
  val incommits_of_channel_value : string


  val eval_fun_of_out_commit: string
    
  (* Runtime functions *)
  val awake : string
  val can_awake : string
    
  (* return the handle value *)
  val get_handle : string
  (* lock the handle *)
  val acquire_handle: string
  val handle_globalrc: string

  val handle_dec_ref_count : string
  val handle_incr_ref_count : string
    
  val fetch_input_commitment : string
  val fetch_output_commitment : string

  val empty_knownSet : string
  val free_knownSet : string
  val knownSet_add : string

  val knownSet_register : string
  val knownSet_forget_all : string
  val knownSet_forget_to_unknown : string
    
  val knownSet_forget : string
  val knownSet_known : string
    
  val register_input_commitment : string
  val register_output_commitment : string

  val commit_list_is_empty : string
    
  (* Thread Synchronization function *)
  val wait_queue_push : string
  val ready_queue_push : string
  val ready_queue_add : string
  val release_all_channels : string
  val acquire : string
  val release : string
  val low_level_yield : string
    
  val generate_channel : string
  val generate_pi_thread : string
    

  (* SchedPool fields *)
  val scheduler : string
  val sched_ready : string
  val sched_wait : string
    
  (* PiThread fields *)
  val pt : string
  val pt_status : string
  val pt_enabled : string
  val pt_known : string
  val pt_env : string
  val pt_commit : string
  val pt_commits : string
  val pt_proc : string
  val pt_pc : string
  val pt_val : string
  val pt_clock : string
  val pt_fuel : string
  val pt_lock : string
  val pt_chans : string  
    
end

(**************************************)
(**************************************)
(**************************************)

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
