(* module Backend
   ---------------

   Backend

*)
(** This module defines the back end part of the compiler. *)

module Make 
  (OutputTypes : SeqAST.OutputTypes) 
  (Names : SeqAST.Names) 
  (Prims : SeqAST.Prims)
  (Printer: SeqAST.PrettyPrinter) =
struct
  
  open Types
  open OutputTypes
  open Syntax
  open SeqAST
  open Printer
  include SeqASTConstUtil.Sigs (OutputTypes) (Names)

  module PrimUtils = PrimitiveUtils.Make(Prims)

  let make_prim =
    fun module_name prim_name arity ->
      SeqASTConstUtil.makeFun 
	(PrimUtils.get_value_name module_name prim_name) 
	pt_value (SeqASTConstC.make_list_n pt_value arity)
  
  (* This module ref is used by the spawn action to retrieve the corresponding
     definition and some values like esize. *)
  
  let compiled_module = ref None

  let lookup d =
    match !compiled_module with
    | None -> failwith "No module compiled"
    | Some m -> let (Def d) = m#lookupDef d in d
					    
  let eval_funs = ref []    
  
  let add_eval_def d = eval_funs := d :: !eval_funs

  let get_eval_name =
    let cpt = ref 0 in
    (fun () -> cpt := succ !cpt;
      "eval_" ^ (string_of_int !cpt))
		
  let make_label, init_label =
    let cpt = ref 0 in
    (fun () -> 
      cpt := succ !cpt; 
      !cpt),
    (fun () -> cpt := 0)


  let def_label_pattern m d = (Printf.sprintf "%s_%s_begin" m d)
    
  let make_ite e i1 i2 = Ite (e, i1, i2) (* nicer declaration and indentation *)
  let make_it e i = Ite (e, i, [])

  let string_of_expr e =
    print_expr Format.str_formatter e;
    Format.flush_str_formatter ()

  class type common_prim_type = object
    method moduleName : string
    method primName : string
    method arity : int
    method args : value list
  end

  let arg_list n =
    let rec f n acc =
      if n = 0 then Var (args 0) :: acc
      else f (n - 1) (Var (args n) :: acc)
    in
    f (n - 1) []

  let init_arg_list n =
    let rec f n acc =
      if n = 0 then Assign (args 0, Val arg_init_value) :: acc
      else f (n - 1) (Assign (args n, Val arg_init_value) :: acc)
    in
    f (n - 1) []
      
  let rec compile_prim0 destination (p: common_prim_type) =
    let f = make_prim p#moduleName p#primName p#arity in
    let arg_l = arg_list p#arity in
    Bloc[
      Declare (args p#arity);
      Seq (init_arg_list p#arity);
      Seq (List.mapi begin fun i v ->
	Seq [compile_value v;
	     CallProc (copy_value, [Var (args i); Var pt_val])]
      end p#args);
      destination f arg_l]

  and compile_value = function 
      (* copy_value is here used as a procedure, 
	 but it returns a bool that can be tested *)
    | VTrue _ -> CallProc (copy_value, [Var pt_val; create_bool true])
    | VFalse _ -> CallProc (copy_value, [Var pt_val; create_bool false])
    | VInt vt -> CallProc (copy_value, [Var pt_val; create_int vt#toVal])
    | VString vt -> CallProc (copy_value, [Var pt_val; create_string vt#toString])
    | VTuple _ -> failwith "TODO compile_value VTuple"
    | VVar vt -> CallProc (copy_value, [Var pt_val; Var (pt_env vt#index)])
    | VPrim p -> compile_prim0 
	(fun f arg_l -> 
	   CallProc (copy_value, [Var pt_val; CallFun (f, arg_l)])) 
	  (p :> common_prim_type)
      
  let compile_end status =
    Seq [
      Comment "------compile_end---------";
      Debug "end";
      Foreach (chan,
	       (CallFun (knownSet_known, [Var pt_known])),
	       [CallProc (handle_dec_ref_count, [CallFun (get_handle, [Var chan])]) ]);
      Foreach (chan,
	       (CallFun (knownSet_forget, [Var pt_known])),
	       [CallProc (handle_dec_ref_count, [CallFun (get_handle, [Var chan])]) ] );
      Assign (pt_status, status);
      return_void]

  and compile_wait =
    (*  property : chans \inter known.FORGET = \emptySet *)
    Seq [
      Comment "------compile_wait---------";
      Debug "wait";
      Assign (pt_pc, invalid_pc);
      Assign (pt_fuel, fuel_init);
      Foreach (chan,
	       (CallFun (knownSet_forget, [Var pt_known])),
	       [CallProc (handle_dec_ref_count, [CallFun (get_handle, [Var chan])]);
		CallProc (knownSet_forget_to_unknown, [Var pt_known; Var chan])]);
      Assign (pt_status, status_wait);
      CallProc (wait_queue_push, [Var sched_wait; Var pt]);
      CallProc (release, [Var pt_lock]);
      return_void]

  and compile_yield label =
    Seq [
      Comment "------compile_yield---------";
      Debug ("yield " ^ (string_of_expr label));
      Assign (pt_pc, label);
      Assign (pt_fuel, fuel_init);
      Foreach (chan,
	       (CallFun (knownSet_forget, [Var pt_known])),
	       [CallProc (handle_dec_ref_count, [CallFun (get_handle, [Var chan])]);
		CallProc (knownSet_forget_to_unknown, [Var pt_known; Var chan])]);
      CallProc (ready_queue_add, [Var sched_ready; Var pt]);
      return_void]
      

  let eot_label () = Printf.sprintf "end_of_try_%d" (make_label ())

  let compile_try_tau = 
    Seq[
      Debug "tau";
      Assign (try_result, try_enabled)]
    
  let compile_try_in (action:in_action_type) = 
    let ok = ok_name, commit_status_enum in
    let vl = vl_name, pt_value in
    let pt_env_x = pt_env action#variableIndex in
    let pt_env_c = pt_env action#channelIndex in
    let label_end_of_try = eot_label () in
    Seq [
      Bloc [
	Comment "------compile_try_in---------";
	Debug (action#toString);
	make_it (CallFun (knownSet_add, [Var pt_chans;  Var pt_env_c]))
	  [CallProc (acquire_handle, [Var (pt_env action#channelIndex)])];
	
	make_it (Op (Equal, CallFun (handle_globalrc, [Var pt_env_c]), Val ("1", prim_int)))
	  [Assign (try_result, try_disabled);
	   Goto label_end_of_try];
	Declare ocommit_var;
	Declare ok;
	
	DoWhile begin [
	  Assign (ocommit_var, CallFun (fetch_output_commitment, [Var pt_env_c] ));
	  
	  make_it (Op (Equal, Var ocommit_var, Val null))
      	    [Assign (try_result, try_commit);
      	     Goto label_end_of_try];
	  
	  DoWhile begin 
	    [Assign (ok, CallFun (can_awake, [Var ocommit_thread; Var ocommit_var]));
      	     make_it (Op (Equal, Var ok, commit_cannot_acquire))
      	       [CallProc (low_level_yield,[])]],
      	    (Op (Equal, Var ok, commit_cannot_acquire)) end;
	  
	  make_it (Op (Equal, Var ok, commit_valid))
      	    [ Declare vl;
	      Declare eval_asvar;
	      Assign (eval_asvar, CallFun (eval_fun_of_out_commit, [Var ocommit_var]));
	      Assign (vl, CallFun (eval_asvar, [Var ocommit_thread]));
	      Assign (pt_env_x, Var vl);
	      (match action#variableType with
	      | TChan _ -> 
		Seq [make_it (CallFun (knownSet_register, [Var pt_known; Var pt_env_x]))
		    [CallProc (handle_incr_ref_count, [CallFun (get_handle, [Var pt_env_x])] )]]
	      | _ -> Seq []);
	      CallProc (awake, [Var scheduler; Var ocommit_thread; Var ocommit_var]); 
      	      Assign (try_result, try_enabled);
      	      Goto label_end_of_try]],
	  
	  (Opu (Not, CallFun (commit_list_is_empty, 
			      [ CallFun (outcommits_of_channel_value, [Var pt_env_c]) ]))) 
	end];
      Label label_end_of_try] 
  (* Label must be out of the bloc to make the c code compile*)
      


  let compile_try_out (action:out_action_type) = 
    let ok = ok_name, commit_status_enum in
    let label_end_of_try = eot_label () in
    let pt_env_c = pt_env action#channelIndex in
    Seq[
      Bloc [
	Comment "------compile_try_out---------";
	Debug (action#toString);
	make_it (CallFun (knownSet_add, [Var pt_chans; Var pt_env_c]))
	  [CallProc (acquire_handle, [Var (pt_env action#channelIndex)])];
	make_it (Op (Equal, CallFun (handle_globalrc, [Var pt_env_c]), Val ("1", prim_int)))
	  [Assign (try_result, try_disabled);
	   Goto label_end_of_try];
	Declare icommit_var;
	Declare ok;
	DoWhile begin [
	  Assign (icommit_var, CallFun (fetch_input_commitment, [Var pt_env_c] ));
	  
	  make_it (Op (Equal, Var icommit_var, Val null))
      	    [Assign (try_result, try_commit);
      	     Goto label_end_of_try];
	  
	  DoWhile begin 
	    [Assign (ok, CallFun (can_awake, [Var icommit_thread; Var icommit_var]));
      	     make_it (Op (Equal, Var ok, commit_cannot_acquire))
      	       [CallProc (low_level_yield,[])]],
      	    (Op (Equal, Var ok, commit_cannot_acquire)) end;
	  
	  make_it (Op (Equal, Var ok, commit_valid))
      	    [ compile_value action#value;
      	      Assign (icommit_thread_env_rv, Var pt_val);
      	      CallProc (awake, [Var scheduler; Var icommit_thread; Var icommit_var]);
      	      Assign (try_result, try_enabled);
      	      Goto label_end_of_try]],
	  (Opu (Not, CallFun (commit_list_is_empty, 
			      [CallFun (incommits_of_channel_value, [Var pt_env_c]) ])))
	end];
	Label label_end_of_try]

      
  let compile_try_new (action:new_action_type) = 
    Bloc [
      Comment "------compile_try_new---------";
      Debug (action#toString);
      Assign (pt_env action#variableIndex, CallFun (create_channel_value, 
						    [CallFun (generate_channel, [])]));
      CallProc (knownSet_register, [Var pt_known; Var (pt_env action#variableIndex)]);
      Assign (try_result, try_enabled)]
      
  let compile_try_spawn (action:spawn_action_type) = 
    let d = lookup action#defName in

    let args_mapper i arg =
      Seq [ compile_value arg;
	    Assign (args i, Var pt_val);
	    Assign (pt_val, Val no_value);
	    (match (value_type_of_value arg)#ofType with
	    | TChan _ -> 
	      Seq [ CallProc (knownSet_register, [Var child_known; Var (args i)]);
		    (*Increments the channel reference count because it is used by a new thread.
		      Not present in the spec.*)
		    CallProc (handle_incr_ref_count, [CallFun (get_handle, [Var (args i)])])]
	    | _ -> Seq []);
	    Assign ((child_env i), Var (args i));
	  ]
    in
    Bloc[
      Comment "------compile_try_spawn---------";
      Debug (action#toString);
      Declare (args action#arity);
      Declare child;
      Assign (child, CallFun (generate_pi_thread, [Val (string_of_int (d#esize), prim_int);
						   Val (string_of_int (d#nbChannels), prim_int);
						   Val (string_of_int (d#nbChoiceMax), prim_int)]));
     
      Seq (List.mapi args_mapper action#args);
      
      Assign (child_proc, Val (action#moduleName ^ "_" ^ action#defName, pdef));
      Assign (child_pc, Val pc_label_init);
      Assign (child_status, status_run);
      CallProc (ready_queue_push, [Var sched_ready; Var child]);
      Assign (try_result, try_enabled)
    ]

  let compile_try_prim (action:prim_action_type) = 
    Seq[
      Debug ( action#toString );
      compile_prim0 (fun f arg_l -> CallProc (f, arg_l)) (action :> common_prim_type);
      Assign (try_result, try_enabled)]

  let compile_try_let (action:let_action_type) = 
    Seq 
      [Debug ( action#toString );
       compile_value action#value;
       Assign (pt_env action#variableIndex, Var pt_val)]

  let compile_try_action = function
    | Tau action -> compile_try_tau
    | Output action -> compile_try_out action
    | Input action -> compile_try_in action
    | New action -> compile_try_new action
    | Spawn action -> compile_try_spawn action
    | Prim action -> compile_try_prim action
    | Let action -> compile_try_let action
    
  let rec compile_process m d proc =
    match proc with
    | Term p -> compile_end status_ended
    | Call p -> compile_call m d p
    | Choice p -> compile_choice m d p

  and compile_choice m d p = 
    let nb_disabled = nb_disabled_name, prim_int
    and def_label = def_label_pattern m#name d#name
    and choice_cont = Array.make p#arity 0
    in
    for i = 0 to (p#arity - 1) do 
      choice_cont.(i) <- make_label ()
    done;
    
    let guard_mapper = (fun i b ->
      let cont_pc = Val ((string_of_int choice_cont.(i)), pc_label) in
      Seq[
	compile_value b#guard; 
	Assign ((pt_enabled i), CallFun (bool_of_bool_value,[Var pt_val]));
	Assign (pt_val, Val no_value);
	make_ite (Var (pt_enabled i))
	  [ compile_try_action b#action;
	    make_ite 
	      (Op (Equal, Var try_result, try_disabled))
	      
	      [Assign ((pt_enabled i), Val prim_false);
	       p_inc nb_disabled ]
	      
	      [make_it (Op (Equal, Var try_result, try_enabled))
		  [p_dec pt_fuel;
		   make_it (Op (Equal, Var pt_fuel, Val zero ))
		     [CallProc (release_all_channels, [Var pt_chans]);
		      CallProc (free_knownSet, [Var pt_chans]);
		      Assign (pt_chans, CallFun (empty_knownSet, []));
		      compile_yield cont_pc];
		   Assign (pt_pc, cont_pc);
		   Goto def_label]]
	  ]
	  [p_inc nb_disabled]])
      
    and action_mapper = (fun i b ->
      let pc = Val ((string_of_int choice_cont.(i)), pc_label) in 
      let if_body = 
	match b#action with
	| Output a ->
	  
	  let eval = SimpleName (get_eval_name ()), eval_ty in
	  add_eval_def (DeclareFun (eval, [string_name_of_varDescr pt],
				    [compile_value a#value; Return (Var pt_val)]));
	  
	  [CallProc (register_output_commitment, 
		     [Var pt; Var (pt_env a#channelIndex); Var eval; pc])]
	    
	| Input a -> 
	  [CallProc (register_input_commitment, 
		     [Var pt; Var (pt_env a#channelIndex);
		      Val (string_of_int a#variableIndex, prim_int); pc])]
	| _ -> []
      in
      make_it (Var (pt_enabled i)) if_body
    )
    in
    Bloc (* cvar after_wait_fuel : label *)
      [(* Debug ("C(" ^ p#toString ^ ")"); *)
       Declare try_result ;
       Assign (try_result, try_result_init);
       Declare nb_disabled ;
       Assign (nb_disabled, Val zero);
       (* Declare chans; *)
       Assign (pt_chans, CallFun (empty_knownSet, []));
       
       Seq (List.mapi guard_mapper p#branches);
       
       make_it ( Op (Equal, Var nb_disabled, Val (string_of_int p#arity, prim_int) ))
	 [ CallProc (release_all_channels, [Var pt_chans]);
	   CallProc (free_knownSet, [Var pt_chans]);
	   Assign (pt_chans, CallFun (empty_knownSet, []));
	   compile_end status_blocked ];
       
       Seq (List.mapi action_mapper p#branches);
       
       CallProc (acquire, [Var pt_lock]);
       CallProc (release_all_channels, [Var pt_chans]);
       CallProc (free_knownSet, [Var pt_chans]);
       Assign (pt_chans, CallFun (empty_knownSet, []));
       compile_wait;
       
       Seq (List.mapi (fun i prefix -> 
	 Seq [ Case (Val (string_of_int choice_cont.(i), prim_int));
	       compile_process m d prefix#continuation]
       ) p#branches)]

  and compile_call m d p =
    
    let rec init_env acc i argsTypes =
      match argsTypes with
      | [] -> acc
      | arg::tl -> 
	let assign = Assign ((pt_env i), Var (args i)) in
	let acc' = acc@
	  [ match arg with
	  | TChan _ -> Seq [ assign; CallProc (knownSet_register, 
					       [Var pt_known; Var (args i)]) ]
	  | _ -> assign ]
	in
	init_env acc' (i+1) tl
    in	      
    
    Bloc [
      Comment "------compile_call---------";
      Debug ("Call(" ^ p#toString ^ ")");
      Declare (args p#arity);
      CallProc (knownSet_forget_all, [ Var pt_known ]);

      Seq (List.mapi 
	     (fun i v -> Seq [ compile_value v; Assign ((args i), Var pt_val); 
			       Assign (pt_val, Val no_value)])
	     p#args);
      
      Seq (init_env [] 0 p#argTypes);
      
      Assign (pt_proc , Var (SimpleName (p#moduleName ^ "_" ^ p#defName), pdef));
      Assign (pt_pc, d_entry);
      Assign (pt_status, status_call);
      return_void]
      
  let compile_def m (Def d) =
    init_label ();
    DeclareFun ((SimpleName (m#name ^ "_" ^ d#name), pdef), [string_name_of_varDescr scheduler; 
							     string_name_of_varDescr pt],
		[Label (def_label_pattern m#name d#name);
		 Switch (Var pt_pc, [Case d_entry;
				     compile_process m d d#process
				    ])])

  let compile_module (Module m) =
    let defs = m#definitions in 
    compiled_module := Some m;
    let (Def d) = List.(nth defs ( (length defs) - 1) ) in
    d, Seq [Seq !eval_funs; Seq (List.map (compile_def m) defs)]
      

  let print_piccType  = Printer.print_piccType

  let print_binop = Printer.print_binop

  let print_varName = Printer.print_varName 
    
  let print_expr = Printer.print_expr 
    
  let print_instr = Printer.print_instr 

  let print_instr_list_std = Printer.print_instr_list_std 

  let print_main = Printer.print_main

end
