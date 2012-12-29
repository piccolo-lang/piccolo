
open Types;;
open Syntax;;

open SeqAST;;
open SeqASTConst;;

let make_label, init_label =
  let cpt = ref 0 in
  (fun () -> 
    cpt := succ !cpt; 
    !cpt),
  (fun () -> cpt := 0)

let def_label_pattern m d = (Printf.sprintf "%s_%s_begin" m d)

let def_fun name = SimpleName name, pdef

let d_entry = Val ("0", pint)

let simple_desc name typ =
  (SimpleName name, typ)
and assign_val ((name, typ) as varDesc) value =
  Assign (varDesc, Val (value, typ))

let make_ite e i1 i2 = Ite (e, i1, i2) (* nicer declaration and indentation *)
let make_it e i = Ite (e, i, [])


let chan = (simple_desc "chan" channel)  (*/!\ en C écrire les foreach dans des blocs *)

let compile_value = function
  | VTrue _ -> Assign (pt_val, Val ("true", pbool))
  | VFalse _ -> Assign (pt_val, Val ("false", pbool))
  | VInt vt -> Assign (pt_val, Val (vt#toString, pint))
  | VString vt -> Assign (pt_val, Val (vt#toString, pstring))
  | VTuple _ -> failwith "TODO compile_value VTuple"
  | VVar vt -> Assign (pt_val, Var (pt_env vt#index) )
  | VPrim _ -> failwith "TODO compile_value VPrim"

and compile_end status =
  Seq [
    Foreach (chan,
	     (CallFun (knows_set_knows, [Var pt_knows])),
	     [CallProc (channel_dec_ref_count, [Var chan]) ]);
    Foreach (chan,
	     (CallFun (knows_set_forget, [Var pt_knows])),
	     [CallProc (channel_dec_ref_count, [Var chan]) ]);
    Assign (pt_status, status);
    return_void]

and compile_wait chans =
  (*  property : chans \inter knows.FORGET = \emptySet  
      [TOASK] -> Maxence, fun intersection de deux ensemble
      C guys -> gestion d'un assert ??
  *)
  Seq [
    Assign (pt_pc, invalid_pc);
    Assign (pt_fuel, fuel_init);
    Foreach (chan,
	     (CallFun (knows_set_forget, [Var pt_knows])),
	     [CallProc (channel_dec_ref_count, [Var chan]);
	      CallProc (knows_set_forget_to_unknown, [Var pt_knows; Var chan])]);
    Assign (pt_status, status_wait);
    CallProc (wait_queue_push, [Var sched_wait; Var pt]);
    CallProc (release, [Var pt_lock]);
    return_void]
and compile_yield label =
  Seq [
    Assign (pt_pc, label);
    Assign (pt_fuel, fuel_init);
    Foreach (chan,
	     (CallFun (knows_set_forget, [Var pt_knows])),
	     [CallProc (channel_dec_ref_count, [Var chan]);
	      CallProc (knows_set_forget_to_unknown, [Var pt_knows; Var chan])]);
    CallProc (ready_queue_add, [Var sched_ready; Var pt]);
    return_void]
    

let rec compile_process m d proc =
  match proc with
  | Term p -> compile_end status_ended
  | Call p -> compile_call m d p
  | Choice p -> compile_choice m d p

and compile_choice m d p = 
  let try_result = simple_desc "tryresult" try_result_enum
  and nb_disabled = simple_desc "nbdisabled" pint
  and chans = simple_desc "chans" (pset channel)
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
      Assign ((pt_enabled i), Var pt_val); (* ("pt->enabled["^stri^"]=pt->val.content.as_bool;"); *)
      make_ite (Var (pt_enabled i))
	[(* [TODO] compileTryAction('a_i, chans) *)
      	  make_ite 
	    (Op (Equal, Var try_result, try_disabled))
	    
	    [assign_val (pt_enabled i) "false";
	     p_inc nb_disabled ]
	    
	    [make_it (Op (Equal, Var try_result, try_enabled))
		[p_dec pt_fuel;
		 make_it (Op (Equal, Var pt_fuel, Val ("0", pint) ))
		   [CallProc (release_all_channels, [Var chans]);
		    compile_yield cont_pc];
		 
		 (*[TODO/TOASK] modifier la spec ? ou solution trop spécifique à c ? *)
		 Assign (pt_pc, cont_pc);
		 Goto def_label]]
	]    
	[p_inc nb_disabled]])
 
  and action_mapper = (fun i b ->
    let pc = Val ((string_of_int choice_cont.(i)), pc_label) in 
    let if_body = 
      match b#action with
      | Output a ->
	let eval = (simple_desc "eval" eval_ty) in
	[DeclareFun (eval, [compile_value a#value; Return (Var pt_val)]);
	 CallProc (register_output_commitment, 
		   [Var pt; Var (pt_env a#channelIndex); Var eval; pc])]
	  
      | Input a -> 
	[CallProc (register_input_commitment, 
		   [Var pt; Var (pt_env a#channelIndex);
		    Val (string_of_int a#variableIndex, pint); pc])]
      | _ -> []
    in
    make_it (Var (pt_enabled i)) if_body
  )
  in
  Bloc (* cvar after_wait_fuel : label *)
    [Declare try_result ;
     Declare nb_disabled ;
     assign_val nb_disabled "0";
     Declare chans;
     
     Seq (List.mapi guard_mapper p#branches);
     
     make_it ( Op (Equal, Var nb_disabled, Val (string_of_int p#arity, pint) ))
       [ CallProc (release_all_channels, [Var chans]);
	 compile_end status_blocked ];
     
     Seq (List.mapi action_mapper p#branches);
     
     CallProc (acquire, [Var pt_lock]);
     CallProc (release_all_channels, [Var chans]);
     compile_wait chans; (* /!\ unused parameter !! -> assert  cf compile_wait *)
     
     Seq (List.mapi (fun i prefix -> 
       Seq [ Case (Val (string_of_int choice_cont.(i), pint));
	     compile_process m d prefix#continuation]
     ) p#branches)]
(* dans un switch case, le case peut être imbriqué dans plusieurs bloc !!

    on aura:
       def(){ 
       
       label @def:

       switch ()
       case def_id :

          { Choice X 
       
            pt.pc <- cont_X
            GOTO @def;
 
            case cont_X:

            case cont_Y:
       
          }
      
       }
    *)
and compile_call m d p =
  
  let args i= (ArrayName (SimpleName "args", i), pvalue) in
  
  let rec init_env acc i argsTypes =
    match argsTypes with
    | [] -> acc
    | arg::tl -> 
      let assign = Assign ((pt_env i), Var (args i)) in
      let acc' = acc@
	[ match arg with
	| TChan _ -> Seq [ assign; CallProc (knows_register, [Var pt_knows; Var (args i)]) ]
	| _ -> assign ]
      in
      init_env acc' (i+1) tl
  in	      
  
  Bloc [
    Declare (args p#arity);
    CallProc (knows_set_forget_all, [ Var pt_knows ]);

    Seq (List.mapi 
	   (fun i v -> Seq [ compile_value v; Assign ((args i), Var pt_val)])
	   p#args);
    
    Seq (init_env [] 0 p#argTypes);
    
    Assign (pt_proc , Var (def_fun (m#name ^ "_" ^ d#name)));
    Assign (pt_pc, d_entry);
    Assign (pt_status, status_call);
    return_void]
       
let compile_def m (Def d) =
  init_label ();
  DeclareFun (def_fun (m#name ^ "_" ^ d#name),
	      [Switch (Var pt_pc, [Case d_entry;
			       compile_process m d d#process
			      ])])

let compile_module (Module m) =
  Seq (List.map (compile_def m) m#definitions)
	   

(* class c_printer : ASTUtils.iter_fold_node = *)
(* let tprint i str = *)
(*   print_endline ((String.make i '\t')^str) *)
(* in *)
(* let compile_value t v = *)
(*   match v with *)
(*   | VTrue _ -> tprint t "pt->val.content.as_bool=true;" *)
(*   | VFalse _ -> tprint t "pt->val.content.as_bool=false;" *)
(*   | VInt vt -> tprint t ("pt->val.content.as_int="^(string_of_int vt#toVal)^";") *)
(*   | VString _ -> tprint t "/* string expr TODO */" *)
(*   | VTuple _ -> tprint t "/* tuple expr TODO */" *)
(*   | VVar vt -> tprint t ("pt->val=pt->env["^(string_of_int vt#index)^"];") *)
(*   | VPrim _ -> tprint t "/* prim expr TODO */" *)

(* and compile_end t status = *)
(*   tprint t "PIT_KnownsSet knows = PIT_knows_set_knows(pt->knowns);"; *)
(*   tprint t "//foreach chan in knows -> macro ?"; *)
(*   tprint t "PIT_channel_dec_ref_count(chan, /*? error ?*/);"; *)
(*   tprint t "//end foreach"; *)
(*   tprint t "knows = PIT_knows_set_forget(pt->knowns);"; *)
(*   tprint t "//foreach chan in knows -> macro ?"; *)
(*   tprint t "PIT_channel_dec_ref_count(chan, NULL);"; (\* NULL = pointeur optionnel pour les erreurs *\) *)
(*   tprint t "//end foreach"; *)
(*   tprint t ("pt->status = "^status^";"); *)
(*   tprint t "return;" *)
(* in *)
(* object(self) *)
(*   (\* faire un compiler fold node héritant de abstract_iter_fold_node *)
(*      et qui fait appeler la méthode truc_pre par la méthode truc_val ?? *)
(*      sémantiquement ce serait plus agréable à lire *)
(*   *\) *)
(*   inherit ASTUtils.abstract_iter_fold_node_repr 0 *)
  
(*   (\* module *\) *)
(*   method moduleDef m rs = self#moduleDef_post m *)
(*   method moduleDef_val m = () *)
(*   method moduleDef_pre m = () *)
(*   method moduleDef_post m = () *)
  
(*   (\* definition *\) *)
(*   method definition_val v m d = *)
(*     tprint 0 "//D_entry: Dynlabel"; *)
(*     tprint 0 "//D_entry <- GenerateLabel()"; *)
(*     tprint 0 ("void "^m#name^"_"^d#name^"(PIT_SchedPool* scheduler, PIT_PiThread *pt){"); *)
(*     tprint 1 "switch(pt->pc){"; *)
(*     tprint 2 "case @D_entry:" *)
      
(*   method definition_post m d = *)
(*     tprint 1 "}";(\* end switch *\) *)
(*     tprint 0 "}" (\* end fun def*\) *)

(*   (\* process *\) *)
(*   method choice v m d p rs = self#choice_post m d p *)
(*   method choice_val v m d p = *)
(*     tprint 2 "{/* Compiled choice */"; *)
(*     (\* cvar after_wait_fuel : label *\) *)
(*     tprint 3 "PIT_try_result_t tryresult;"; *)
(*     tprint 3 "// enum {TRY_ENABLED, TRY_DISABLED, TRY_COMMIT} PIT_try_result_t;"; *)
(*     tprint 3 "int nbdisabled = 0;"; *)
(*     (\* cvar choice_cont : Array[dynLabel] *\) *)
(*     tprint 3 "PIT_Set chans = PIT_Set_empty_set();//[C] Ask C guys"; *)

(*     let branch_iterator = (fun i b -> *)
(*       let stri = (string_of_int i) in *)
(*       (\* choice_cont <- generate_label *\) *)
(*       compile_value 3 b#guard;  *)
(*       tprint 3 ("pt->enabled["^stri^"]=pt->val.content.as_bool;"); *)
(*       tprint 3 ("if( pt->enabled["^stri^"]){"); *)
(*       (\* compileTryAction('a(i), chans) *\) *)
(*       tprint 4 "if( tryresult == TRY_DISABLED ){"; *)
(*       tprint 5 ("pt->enabled["^stri^"]=false;"); *)
(*       tprint 5 "nbdisabled++;"; *)
(*       tprint 4 "else if (tryresult == TRY_ENABLED){"; *)
(*       tprint 5 "pt->fuel--;"; *)
(*       tprint 5 "if(pt->fuel == 0){"; *)
(*       tprint 6 "PIT_release_all_channels(chans); //[C] check function Name"; *)
(*       (\* compileYield *\) *)
(*       tprint 5 "}"; *)
(*       tprint 5 ("goto @choice_cont["^stri^"]; // !!!"); *)
(*       tprint 4 "}"; *)
(*       tprint 3 "else{"; *)
(*       tprint 4 "nbdisabled++;"; *)
(*       tprint 3 "}" *)
(*     ) in *)
(*     List.iteri branch_iterator p#branches; *)

(*     tprint 3 ("if( nbdisabled == "^(string_of_int p#arity)^" ){"); *)
(*     tprint 4 "PIT_release_all_channels(chans); //[C] check function name"; *)
(*     compile_end 4 "STATUS_BLOCKED"; *)
(*     tprint 3 "}"; *)
    
(*     let branch_iterator = (fun i b -> *)
(*       let stri = (string_of_int i) in *)
(*       tprint 3 ("if( pt->enabled["^stri^"] ){"); *)
(*       (match b#action with *)
(*       | Output a -> *)
(* 	tprint 4 "PIT_EvalFunction eval(PIT_PiThread *pt){"; *)
(* 	compile_value 5 a#value; *)
(* 	tprint 5 "return pt->val;//[C] changer en retour de pointer, si kind non utilisé dans la runtime"; *)
(* 	tprint 5 "               // changer PIT_value pour que ce ne soit qu'une union ??"; *)
(* 	tprint 4 "};"; *)
(* 	let c_index= string_of_int a#channelIndex in *)
(* 	tprint 4 ("PIT_register_output_commitment(pt, pt->env["^c_index^"].content.as_channel,eval, /*? choice_cont["^stri^"]?*/);") (\* choice_cont[i] *\) *)

(*       | Input a ->  *)
(* 	let c_index= string_of_int a#channelIndex in *)
(* 	tprint 4 ("PIT_register_input_commitment(pt, pt->env["^c_index^"].content.as_channel, /*? choice_cont["^stri^"]?*/);") (\* choice_cont[i] *\) *)

	
(*       | _ -> assert false); *)
(*       tprint 3 "}" *)
(*     ) in *)
(*     List.iteri branch_iterator p#branches; *)

(*     tprint 3 "PIT_acquire(pt->lock); //[C] check function Name"; *)
(*     tprint 3 "PIT_release_all_channels(chans); //[C] check function Name"; *)
    
(*     (\*Compile wait*\) *)
    
(*     (\* *)
(*       cfor i : int from 0 to n − 1 *)
(*          case @choice_cont[i] : *)
(*          Compile(Pi ) *)
(*       cendfor *)
(*     *\) *)
(*     tprint 2 "}" *)
      
(*   method branch v m d p index b g a q = self#branch_post m d p index b *)
(*   method branch_val v m d p index b = () *)
(*   method branch_pre m d p index b = () *)
(*   method branch_post m d p index b = () *)

(*   (\* method call v m d p rs = self#call_post m d p *\) *)
(*   method call_post m d p = *)
(*     tprint 2 "{/* D(v1:T1, ...,vn:Tn) */"; *)
(*     tprint 3 ("PIT_value args["^(string_of_int p#arity)^"];"); *)
(*     tprint 3 "PIT_knows_set_forget_all(pt->knows);"; *)
(*     tprint 3 "/*[CHECK] La copie de structure est censé fonctionné*/"; *)
(*     List.iteri *)
(*       (fun i v -> *)
(* 	compile_value 3 v; *)
(* 	tprint 3 ("args["^(string_of_int i)^"]=pt->val;") *)
(*     ) p#args; *)
(*     (\*[CHECK/TOASK] ne peut on pas directement affecté dans pt->env ?? *\) *)
(*     List.iteri *)
(*       (fun i t -> *)
(* 	let stri = (string_of_int i) in *)
(* 	tprint 3 ( "pt->env["^stri^"] = args["^stri^"];"); *)
(* 	match t with *)
(* 	  TChan _ -> tprint 3 ("PIT_knows_register(pt->knows, args["^stri^"].content.as_channel);") *)
(* 	| _ -> () *)
(*       ) p#argTypes; *)
(*     tprint 3 "pt->proc = D; // find what is D "; *)
(*     tprint 3 "pt->pc = @D_entry // same"; *)
(*     tprint 3 "pt->status = STATUS_CALL;"; *)
(*     tprint 3 "return;"; *)
(*     tprint 2 "}" *)

(*  (\* Reqmarque pour le code C: signatures sans pointeurs ??*\) *)
(*   method term_post m d p = () *)
    
(*   (\* action *\) *)
(*   method outAction v m d p a r = self#outAction_post m d p a *)
(*   method outAction_val v m d p a = () *)
(*   method outAction_pre m d p a = () *)
(*   method outAction_post m d p a = () *)
(*   method inAction v m d p a = self#inAction_post m d p a *)
(*   method inAction_val v m d p a = () *)
(*   method inAction_pre m d p a = () *)
(*   method inAction_post m d p a = () *)

(*   method tauAction_post m d p a = *)
(*     tprint 1 "try_result=STATUS;" *)

(*   method newAction v m d p a = self#newAction_post m d p a *)
(*   method newAction_val v m d p a = () *)
(*   method newAction_pre m d p a = () *)
(*   method newAction_post m d p a = () *)
(*   method spawnAction v m d p a rs = self#spawnAction_post m d p a *)
(*   method spawnAction_val v m d p a = () *)
(*   method spawnAction_pre m d p a = () *)
(*   method spawnAction_post m d p a = () *)
(*   method primAction v m d p a rs = self#primAction_post m d p a *)
(*   method primAction_val v m d p a = () *)
(*   method primAction_pre m d p a = () *)
(*   method primAction_post m d p a = () *)
(*   method letAction v m d p a r = self#letAction_post m d p a *)
(*   method letAction_val v m d p a = () *)
(*   method letAction_pre m d p a = () *)
(*   method letAction_post m d p a = () *)
(* end *)

(* let pass m =  *)
(*   ASTUtils.fold_module m ((new c_printer ):> (unit, unit) ASTUtils.fold_node) *)
