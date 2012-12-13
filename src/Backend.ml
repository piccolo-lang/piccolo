
open Types;;
open Syntax;;


class c_printer : ASTUtils.iter_fold_node =
let tprint i str =
  print_endline ((String.make i '\t')^str)
in
let compile_value t v =
  match v with
  | VTrue _ -> tprint t "pt->val.content.as_bool=true;"
  | VFalse _ -> tprint t "pt->val.content.as_bool=false;"
  | VInt vt -> tprint t ("pt->val.content.as_int="^(string_of_int vt#toVal)^";")
  | VString _ -> tprint t "/* string expr TODO */"
  | VTuple _ -> tprint t "/* tuple expr TODO */"
  | VVar vt -> tprint t ("pt->val=pt->env["^(string_of_int vt#index)^"];")
  | VPrim _ -> tprint t "/* prim expr TODO */"

and compile_end t status =
  tprint t "PIT_KnownsSet knows = PIT_knows_set_knows(pt->knowns);";
  tprint t "//foreach chan in knows -> macro ?";
  tprint t "PIT_channel_dec_ref_count(chan, /*? error ?*/);";
  tprint t "//end foreach";
  tprint t "knows = PIT_knows_set_forget(pt->knowns);";
  tprint t "//foreach chan in knows -> macro ?";
  tprint t "PIT_channel_dec_ref_count(chan, NULL);"; (* NULL = pointeur optionnel pour les erreurs *)
  tprint t "//end foreach";
  tprint t ("pt->status = "^status^";");
  tprint t "return;"
in
object(self)
  (* faire un compiler fold node héritant de abstract_iter_fold_node
     et qui fait appeler la méthode truc_pre par la méthode truc_val ??
     sémantiquement ce serait plus agréable à lire
  *)
  inherit ASTUtils.abstract_iter_fold_node_repr 0
  
  (* module *)
  method moduleDef m rs = self#moduleDef_post m
  method moduleDef_val m = ()
  method moduleDef_pre m = ()
  method moduleDef_post m = ()
  
  (* definition *)
  method definition_val v m d =
    tprint 0 "//D_entry: Dynlabel";
    tprint 0 "//D_entry <- GenerateLabel()";
    tprint 0 ("void "^m#name^"_"^d#name^"(PIT_SchedPool* scheduler, PIT_PiThread *pt){");
    tprint 1 "switch(pt->pc){";
    tprint 2 "case @D_entry:"
      
  method definition_post m d =
    tprint 1 "}";(* fin switch *)
    tprint 0 "}" (* fin fun def*)

  (* process *)
  method choice v m d p rs = self#choice_post m d p
  method choice_val v m d p =
    tprint 2 "{/* Compiled choice */";
    (* cvar after_wait_fuel : label *)
    tprint 3 "PIT_try_result_t tryresult;";
    tprint 3 "// enum {TRY_ENABLED, TRY_DISABLED, TRY_COMMIT} PIT_try_result_t;";
    tprint 3 "int nbdisabled = 0;";
    (* cvar choice_cont : Array[dynLabel] *)
    tprint 3 "PIT_Set chans = PIT_Set_empty_set();//[C] Ask C guys";
    let branch_iterator = (fun i b ->
      let stri = (string_of_int i) in
      (* choice_cont <- generate_label *)
      compile_value 3 b#guard; 
      tprint 3 ("pt->enabled["^stri^"]=pt->val.content.as_bool;");
      tprint 3 ("if( pt->enabled["^stri^"]){");
      (* compileTryAction('a(i), chans) *)
      tprint 4 "if( tryresult == TRY_DISABLED ){";
      tprint 5 ("pt->enabled["^stri^"]=false;");
      tprint 5 "nbdisabled++;";
      tprint 4 "else if (tryresult == TRY_ENABLED){";
      tprint 5 "pt->fuel--;";
      tprint 5 "if(pt->fuel == 0){";
      tprint 6 "PIT_release_all_channels(chans); //[C] check function Name";
      (* compileYield *)
      tprint 5 "}";
      tprint 5 ("goto @choice_cont["^stri^"]; // !!!");
      tprint 4 "}";
      tprint 3 "else{";
      tprint 4 "nbdisabled++;";
      tprint 3 "}"
    ) in
    List.iteri branch_iterator p#branches;

    tprint 3 ("if( nbdisabled == "^(string_of_int p#arity)^" ){");
    tprint 4 "PIT_release_all_channels(chans); //[C] check function name";
    compile_end 4 "STATUS_BLOCKED";
    tprint 3 "}";
    
    let branch_iterator = (fun i b ->
      let stri = (string_of_int i) in
      tprint 3 ("if( pt->enabled["^stri^"] ){");
      (match b#action with
      | Output a ->
	tprint 4 "//c'est hallucinant mais d'après les tests, ça marche !!!";
	tprint 4 "PIT_EvalFunction eval(PIT_PiThread *pt){";
	compile_value 5 a#value;
	tprint 5 "return pt->val;//[C] changer en retour de pointer, si kind non utilisé dans la runtime";
	tprint 5 "               // changer PIT_value pour que ce ne soit qu'une union ??";
	tprint 4 "};";
	let c_index= string_of_int a#channelIndex in
	tprint 4 ("PIT_register_output_commitment(pt, pt->env["^c_index^"].content.as_channel,eval, /*? choice_cont["^stri^"]?*/);") (* choice_cont[i] *)

      | Input a -> 
	let c_index= string_of_int a#channelIndex in
	tprint 4 ("PIT_register_input_commitment(pt, pt->env["^c_index^"].content.as_channel, /*? choice_cont["^stri^"]?*/);") (* choice_cont[i] *)

	
      | _ -> assert false);
      tprint 3 "}"
    ) in
    List.iteri branch_iterator p#branches;

    tprint 3 "PIT_acquire(pt->lock); //[C] check function Name";
    tprint 3 "PIT_release_all_channels(chans); //[C] check function Name";
    
    (*Compile wait*)
    
    (*
      cfor i : int from 0 to n − 1
         case @choice_cont[i] :
         Compile(Pi )
      cendfor
    *)
    tprint 2 "}"
      
  method branch v m d p index b g a q = self#branch_post m d p index b
  method branch_val v m d p index b = ()
  method branch_pre m d p index b = ()
  method branch_post m d p index b = ()

  (* method call v m d p rs = self#call_post m d p *)
  method call_post m d p =
    tprint 2 "{/* D(v1:T1, ...,vn:Tn) */";
    tprint 3 ("PIT_value args["^(string_of_int p#arity)^"];");
    tprint 3 "PIT_knows_set_forget_all(pt->knows);";
    tprint 3 "/*[CHECK] La copie de structure est censé fonctionné*/";
    List.iteri
      (fun i v ->
	compile_value 3 v;
	tprint 3 ("args["^(string_of_int i)^"]=pt->val;")
    ) p#args;
    (*[CHECK/TOASK] ne peut on pas directement affecté dans pt->env ?? *)
    List.iteri
      (fun i t ->
	let stri = (string_of_int i) in
	tprint 3 ( "pt->env["^stri^"] = args["^stri^"];");
	match t with
	  TChan _ -> tprint 3 ("PIT_knows_register(pt->knows, args["^stri^"].content.as_channel);")
	| _ -> ()
      ) p#argTypes;
    tprint 3 "pt->proc = D; // find what is D ";
    tprint 3 "pt->pc = @D_entry // same";
    tprint 3 "pt->status = STATUS_CALL;";
    tprint 3 "return;";
    tprint 2 "}"

 (* Reqmarque pour le code C: signatures sans pointeurs ??*)
  method term_post m d p = ()
    
  (* action *)
  method outAction v m d p a r = self#outAction_post m d p a
  method outAction_val v m d p a = ()
  method outAction_pre m d p a = ()
  method outAction_post m d p a = ()
  method inAction v m d p a = self#inAction_post m d p a
  method inAction_val v m d p a = ()
  method inAction_pre m d p a = ()
  method inAction_post m d p a = ()

  method tauAction_post m d p a =
    tprint 1 "try_result=STATUS;"

  method newAction v m d p a = self#newAction_post m d p a
  method newAction_val v m d p a = ()
  method newAction_pre m d p a = ()
  method newAction_post m d p a = ()
  method spawnAction v m d p a rs = self#spawnAction_post m d p a
  method spawnAction_val v m d p a = ()
  method spawnAction_pre m d p a = ()
  method spawnAction_post m d p a = ()
  method primAction v m d p a rs = self#primAction_post m d p a
  method primAction_val v m d p a = ()
  method primAction_pre m d p a = ()
  method primAction_post m d p a = ()
  method letAction v m d p a r = self#letAction_post m d p a
  method letAction_val v m d p a = ()
  method letAction_pre m d p a = ()
  method letAction_post m d p a = ()
end


(* class c_printer : [int, unit] ASTUtils.fold_node =  *)
(* let tprint i str = *)
(*   print_endline ((String.make i '\t')^str) *)
(* in *)
(* object(self) *)
(*   (\* config *\) *)
(*   method verbosity = 0 *)
(*   method echo vn str = () (\* if vn<=n then print_string str *\) *)
(*   method echoln vn str = () (\* if vn<=n then print_endline str *\) *)
      
(*   (\* module *\) *)
(*   method moduleDef_val m =  *)
(*     tprint 0 ("/*** Module "^m#name^" ***/"); *)
(*     1 *)
(*   method moduleDef m _ =  *)
(*     tprint 0 ("/*** end of module "^m#name^"***/") *)

(*   (\* definitions *\) *)
(*   method definition_val t m (d:definition_type) =  *)
(*     tprint 1 "//D_entry: Dynlabel"; *)
(*     tprint 1 "//D_entry <- GenerateLabel()"; *)
(*     tprint 1 ("void "^m#name^"_"^d#name^"(PIT_SchedPool* scheduler, PIT_PiThread *pt){"); *)
(*     tprint 2 "switch(pt->pc){"; *)
(*     tprint 3 "case @D_entry:"; *)
(*     3 *)

(*   method definition t m d esize =  *)
(*     tprint 2 "}";(\* fin switch *\) *)
(*     tprint 1 "}" (\* fin fun def*\) *)

(*   (\* processes *\) *)
(*   method choice_val t m d p = t *)
(*   method choice t m d p esizes = () *)
(*   method branch_val t (m:module_type) (d:definition_type) (p:process choice_process_type) (i:int) (b:process prefix_process_type) = t *)
(*   method branch t m d p i b s1 s2 s3 = () *)
(*   method call_val t m d p = t *)
(*   method call t m d p _ =  *)
(*     tprint t "{/* D(v1:T1, ...,vn:Tn) */"; *)
(*     tprint (t+1) ("PIT_value args["^(string_of_int p#arity)^"];"); *)
(*     tprint (t+1) "PIT_knows_set_forget_all(pt->knows);"; *)
(*     tprint (t+1) "/*[CHECK] La copie de structure est censé fonctionné*/"; *)
(*     List.iteri  *)
(*       (fun i v ->  *)
(* 	 compile_value (t+1) v; *)
(* 	tprint (t+1) ("args["^(string_of_int i)^"]=pt->val;") *)
(*     ) p#args; *)
(*     (\*[CHECK/TOASK] ne peut on pas directement affecté dans pt->env ?? *\) *)
(*     List.iteri *)
(*       (fun i ty -> *)
(* 	let stri = (string_of_int i) in *)
(* 	tprint (t+1) ( "pt->env["^stri^"] = args["^stri^"];"); *)
(* 	match ty with *)
(* 	  TChan _ -> tprint 1 ("PIT_knows_register(pt->knows, args["^stri^"].content.as_channel);") *)
(* 	| _ -> () *)
(*       ) p#argTypes; *)
(*     tprint (t+1) "pt->proc = D; // find what is D "; *)
(*     tprint (t+1) "pt->pc = @D_entry // same"; *)
(*     tprint (t+1) "pt->status = STATUS_CALL;"; *)
(*     tprint (t+1) "return;"; *)
(*     tprint t "}" *)

(*   method term_val t m d p = () *)
(*   method term t m d p =  *)
(*         (\* Compile(end, status) ?? *\) *)
(*     tprint 1 "PIT_KnownsSet knows = PIT_knows_set_knows(pt->knowns);"; *)
(*     tprint 1 "//foreach chan in knows -> macro ?"; *)
(*     tprint 1 "PIT_channel_dec_ref_count(chan, /*? error ?*/);"; *)
(*     tprint 1 "//end foreach"; *)
(*     tprint 1 "knows = PIT_knows_set_forget(pt->knowns);"; *)
(*     tprint 1 "//foreach chan in knows -> macro ?"; *)
(*     tprint 1 "PIT_channel_dec_ref_count(chan, NULL);"; (\* NULL = pointeur optionnel pour les erreurs *\) *)
(*     tprint 1 "//end foreach"; *)
(*     tprint 1 "pt->status = ?status? ; // Compile(end, status) ??"; *)
(*     tprint 1 "return;"; *)

(*   (\* actions *\) *)
(*   method outAction_val t m d p a = t *)
(*   method outAction t m d p a r = () *)
(*   method inAction_val t m d p a = ()  *)
(*   method inAction t m d p a = () *)

(*   method tauAction_val t m d p a = () *)
(*   method tauAction t m d p a =  *)
(*     tprint 1 "try_result=STATUS; // demander aux gens C de rajouter l'enum (cf compil guard)" *)

(*   method newAction_val t m d p a = () *)
(*   method newAction t m d p a = () *)

(*   method spawnAction_val t m d p a = t *)
(*   method spawnAction t m d p a rs = () *)
(*   method primAction_val t m d p a = t *)
(*   method primAction t m d p a rs = () *)
(*   method letAction_val t m d p a = t *)
(*   method letAction t m d p a r = () *)

(*   (\* value *\) *)
(*   method trueValue_val t m d p ty v = () *)
(*   method trueValue t m d p ty v = () *)
(*   method falseValue_val t m d p ty v = () *)
(*   method falseValue t m d p ty v = () *)
(*   method intValue_val t m d p ty v = () *)
(*   method intValue t m d p ty v = () *)
(*   method stringValue_val t m d p ty v = () *)
(*   method stringValue t m d p ty v = () *)
(*   method tupleValue_val t m d p ty v = t *)
(*   method tupleValue t m d p ty v rs = () *)
(*   method varValue_val t m d p ty v = () *)
(*   method varValue t m d p ty v = () *)
(*   method primValue_val t m d p ty v = t *)
(*   method primValue t m d p ty v rs = () *)
(* end *)

let pass m = 
  ASTUtils.fold_module m ((new c_printer ):> (unit, unit) ASTUtils.fold_node)
