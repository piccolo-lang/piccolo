
open SeqAST

let makeFun name ret args =
  SimpleName name, Fun (ret, args)

module Sigs (Types: SeqAST.OutputTypes) (Names : SeqAST.Names) =
struct
  open Types

  let copy_value = makeFun Names.copy_value prim_bool [pt_value; pt_value]

  let bool_of_bool_value = makeFun Names.bool_of_bool_value prim_bool [pt_value]

  let outcommits_of_channel_value = makeFun Names.outcommits_of_channel_value commit_list [pt_value]
  let incommits_of_channel_value = makeFun Names.incommits_of_channel_value commit_list [pt_value]


  let eval_fun_of_out_commit = makeFun Names.eval_fun_of_out_commit eval_ty [commit]

  let awake = makeFun Names.awake void [sched_pool; pi_thread; commit] 
  let can_awake = makeFun Names.can_awake commit_status_enum [pi_thread; commit]
  
    
    (* return the handle value *)
  let get_handle = makeFun Names.get_handle handle [pt_value]
    (* lock the handle *)
  let acquire_handle = makeFun Names.acquire_handle void [handle]
  let handle_globalrc = makeFun Names.handle_globalrc prim_int [handle]


  let handle_dec_ref_count = makeFun Names.handle_dec_ref_count void [ channel]
  let handle_incr_ref_count = makeFun Names.handle_incr_ref_count void [ channel]

  let fetch_input_commitment = makeFun Names.fetch_input_commitment commit [channel]
  let fetch_output_commitment = makeFun Names.fetch_output_commitment commit [channel]


  let register_input_commitment = makeFun Names.register_input_commitment 
    void [pi_thread; channel; prim_int; pc_label]

  let register_output_commitment = makeFun Names.register_output_commitment 
    void [pi_thread; channel; eval_ty; pc_label ]

  let commit_list_is_empty = makeFun Names.commit_list_is_empty pt_bool [commit_list]

  (* KnownSet *)

  let empty_knownSet = makeFun Names.empty_knownSet knownSet [] 
  let free_knownSet = makeFun Names.free_knownSet void [knownSet]

  let knownSet_add = makeFun Names.knownSet_add prim_bool [knownSet; knownValue] 

  let knownSet_register = makeFun Names.knownSet_register pt_bool [knownSet; knownValue]
  let knownSet_forget_all = makeFun Names.knownSet_forget_all void [knownSet]
  let knownSet_forget_to_unknown = makeFun Names.knownSet_forget_to_unknown void [knownSet; knownValue]

  let knownSet_forget = makeFun Names.knownSet_forget knownSet [knownSet]
  let knownSet_known = makeFun Names.knownSet_known knownSet [knownSet]


  (* Thread Synchronization function *)
  let wait_queue_push = makeFun Names.wait_queue_push void [wait_queue; pi_thread]
  let ready_queue_push = makeFun Names.ready_queue_push void [ready_queue; pi_thread]
  let ready_queue_add = makeFun Names.ready_queue_add void [ready_queue; pi_thread] 
  let release_all_channels = makeFun Names.release_all_channels void [knownSet]

  let acquire = makeFun Names.acquire void [mutex]
  let release = makeFun Names.release void [mutex]
  let low_level_yield = makeFun Names.low_level_yield void []

  let generate_channel = makeFun Names.generate_channel channel []
  let generate_pi_thread = makeFun Names.generate_pi_thread pi_thread [prim_int; prim_int; prim_int] 
    
    
(* SchedPool fields *)
  let scheduler = SimpleName Names.scheduler, sched_pool
  let sched_ready = (RecordName (scheduler, Names.sched_ready), (ready_queue))
  let sched_wait = (RecordName (scheduler, Names.sched_wait), (wait_queue))

(* PiThread fields *)
  let pt = (SimpleName Names.pt, pi_thread)
  let pt_status =(RecordName (pt, Names.pt_status), status_enum)
  let pt_enabled i = (ArrayName ((RecordName (pt, Names.pt_enabled) ), Val (string_of_int i, prim_int)), pt_bool)
  let pt_known = (RecordName (pt, Names.pt_known), knownSet)
  let pt_env i = (ArrayName ((RecordName (pt, Names.pt_env) ), Val (string_of_int i, prim_int)), pt_value)
  let pt_commit = (RecordName (pt, Names.pt_commit), commit)
  let pt_commits = (RecordName (pt, Names.pt_commits), commit_list)
  let pt_proc = (RecordName (pt, Names.pt_proc), pdef)
  let pt_pc = (RecordName (pt, Names.pt_pc), pc_label)
  let pt_val = (RecordName (pt, Names.pt_val), pt_value)
  let pt_clock = (RecordName (pt, Names.pt_clock), clock)
  let pt_fuel = (RecordName (pt, Names.pt_fuel), prim_int)

  let pt_lock = (RecordName (pt, Names.pt_lock), mutex)
  let pt_chans = (RecordName (pt, Names.pt_chans), knownSet)


  (* local variables *)
  
  let try_result = SimpleName Names.try_result, try_result_enum

  let chan = SimpleName Names.chan, channel
  let chans = SimpleName Names.chans, knownSet
 

  let ocommit_var = SimpleName Names.ocommit_var, commit
  let ocommit_thread = RecordName (ocommit_var, Names.ocommit_thread), pi_thread
  let ocommit_thread_val = RecordName (ocommit_thread, Names.ocommit_thread_val), pt_value
    
  let icommit_var = SimpleName Names.icommit_var, commit 
  let icommit_thread = RecordName (icommit_var, Names.icommit_thread), pi_thread 
  let icommit_in = RecordName (icommit_var, Names.icommit_in), in_commit
  let icommit_refvar = RecordName (icommit_in, Names.icommit_refvar), prim_int 
  let icommit_thread_env_rv = 
    ArrayName (RecordName (icommit_thread, Names.icommit_thread_env_rv), Var icommit_refvar), pt_value 


  let args i= (ArrayName (SimpleName Names.args, Val (string_of_int i, prim_int)), pt_value)
  let child = SimpleName Names.child, pi_thread
  let child_proc = (RecordName (child, Names.child_proc), pdef)
  let child_pc = (RecordName (child, Names.child_pc), pc_label)
  let child_status =(RecordName (child, Names.child_status), status_enum)
  let child_known = (RecordName (child, Names.child_known), knownSet)
  let child_env i = (ArrayName ((RecordName (child, Names.child_env) ), Val (string_of_int i, prim_int)), pt_value)


  (* misc *)
  let return_void = Return (Val ("", void))

  let p_inc v = Assign (v, (Op (Sum, Var v, Val ("1", prim_int))))
  let p_dec v = Assign (v, (Op (Minus, Var v, Val ("1", prim_int))))

  let make_prim_int i = (string_of_int i), prim_int
end
