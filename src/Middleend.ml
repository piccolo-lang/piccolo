(* module Middleend
   ---------------

   Middleend passes

*)
(** This module defines the middle end part of the compiler. *)

open Utils;;
open Syntax;;
open Typing;;

(** Representation of a [string list,int] fold_node. 
    Compute esize and attribute index *)

class env_compute_pass (n:int) : [string list, int] ASTUtils.fold_node =
  let lookup env v =
    let rec aux env n = match env with
      | [] -> None
      | w::env' -> if v=w then Some n else aux env' (n+1)
    in aux env 0
  in
object(self)
  
  (* This list contains the new variables we might run into while walking through the tree. It doesn't contain the variables given as parameters of definitions, only the ones met in Let, In, and New. *)
  val mutable newVar = []

  (* config *)
  method verbosity = n
  method echo vn str = if vn<=n then print_string str
  method echoln vn str = if vn<=n then print_endline str
    
  (* module *)
  method moduleDef_val m = []
  method moduleDef m esizes =
    self#echoln 2 ("\n[ESIZE_MODULE] env pass finished in Module: " ^ m#name);
    self#echoln 2 (string_of_int (list_max esizes));
    self#echoln 2 ("\nmodule esize ---->>> " ^ (String.concat " , " newVar));
    list_max esizes 

  (* definitions *)
  method definition_val _ m (d:definition_type) =
    self#echoln 2 ("\n[ESIZE_DEF] Start : " ^ (d#name) ^ " env start : " );
    d#env
  method definition _ m d esize =
    self#echoln 2 ("\n[ESIZE_DEF] env pass finished in Definition: " ) ;
    self#echoln 2 (d#name ^ " ==> computed env size = " ^ (string_of_int esize)) ;
    d#setEsize esize;
    esize

  (* processes *)
  method choice_val env m d p = env
  method choice env m d p esizes = list_max esizes

  method branch_val env (m:module_type) (d:definition_type) (p:process choice_process_type) (i:int) (b:process prefix_process_type) =
    match b#action with
      | Input a ->
	  (match (lookup env a#channel) with
	     | None -> () (* an error will be return by the typing pass *)
	     | Some n -> a#setChannelIndex n);
	  (match (lookup env a#variable) with
	     | None -> (a#setVariableIndex (List.length env)) ;
		 d#extendEnv a#variable;
		 newVar <- (a#variable)::newVar; (* we add a in the list of new variables *)
		 env @ [a#variable]
	     | Some n -> a#setVariableIndex n ; env)
      | Output a ->
	  (match (lookup env a#channel) with
	    | None -> () (* an error will be return by the typing pass *)
	    | Some n ->  
	      a#setChannelIndex n);
	   env
      | New a ->
	  (match (lookup env a#variable) with
	     | None -> a#setVariableIndex (List.length env) ;
		 d#extendEnv a#variable; 
		 newVar <- (a#variable)::newVar; 
		 env @ [a#variable]
	     | Some n -> a#setVariableIndex n ; env)	    
      | Let a ->  (match (lookup env a#variable) with
		     | None -> a#setVariableIndex (List.length env) ;
		       newVar <- (a#variable)::newVar;
		       d#extendEnv a#variable;
		       env @ [a#variable]
		     | Some n -> a#setVariableIndex n ; env)
      | _ -> env
  method branch env m d p i b s1 s2 s3 = 
    self#echoln 2 ("\nBRANCH : " ^ (string_of_int (s1)) ^  (string_of_int (s2)) ^ (string_of_int (s3)));
    s1+s2+s3
  method call_val env m d p = env
  method call env m d p _ = 
    let Def(def_called) = try
      m#lookupDef (p#defName)
    with Not_found -> failwith "undefined definition called...(calcul esize)"
    in (* sould never happen *)
    let esize_called = def_called#esize in
    if (esize_called > (List.length env)) then (esize_called - (List.length newVar))
    else ((List.length env) - (List.length newVar)) 
  (* We remove the variables met through the top->bottom , they will be added int the bottom->top *)
  method term_val env m d p = ()
  method term env m d p = ((List.length env) - (List.length newVar))

  (* actions *)
  method outAction_val env m d p a = env
  method outAction env m d p a r = 0
  method inAction_val env m d p a = ()
  method inAction env m d p a = 
    (* If the variable is one of the new variables met in the top->bottom, we add it *)
    if (List.mem a#variable newVar) then ( 
      newVar <- List.filter (fun v -> v != a#variable) newVar;
      1)
    else (
      0) 
  method tauAction_val env m d p a =  ()
  method tauAction env m d p a = 0
  method newAction_val env m d p a = ()
  method newAction env m d p a = 
    if (List.mem a#variable newVar) then ( 
      newVar <- List.filter (fun v -> v != a#variable) newVar;
      1 )
    else (
      0)
  method spawnAction_val env m d p a = env
  method spawnAction env m d p a rs = 0
  method primAction_val env m d p a = env
  method primAction env m d p a rs = 0
  method letAction_val env m d p a = env
  method letAction env m d p a r =
    if (List.mem a#variable newVar) then ( 
      newVar <- List.filter (fun v -> v != a#variable) newVar;
      1 )
    else (
      0)

  (* value *)
  method trueValue_val env m d p t v = ()
  method trueValue env m d p t v = 0
  method falseValue_val env m d p t v = ()
  method falseValue env m d p t v = 0
  method intValue_val env m d p t v = ()
  method intValue env m d p t v = 0
  method stringValue_val env m d p t v = ()
  method stringValue env m d p t v = 0
  method tupleValue_val env m d p t v = env
  method tupleValue env m d p t v rs = 0
  method varValue_val env m d p t v =
    match (lookup env v#name) with
      | None -> ()
	  (* generate an error and save in module errors ??? *)
      | Some n -> v#setIndex n 
	  (* or in a further pass, just test if index is still -1 *)
  method varValue env m d p t v = 0
  method primValue_val env m d p t v = env
  method primValue env m d p t v rs = 0
end



(** Representation of a iter_fold_node. Compute and set the Csize value in
    each definition. *)
class csize_compute_pass (n:int) : ASTUtils.iter_fold_node =
object(self)
  inherit ASTUtils.abstract_iter_fold_node_repr n

  (* val mutable _csize_process_map = ProcessMap.empty *)
  (* val mutable _csize_def_map = DefMap.empty (\* deprecated *\) *)
  (* val mutable _csize = -1 *)
  val mutable _current_choice_pile = []

  (* config *)
  method verbosity = n
  method echo vn str = if vn<=n then print_string str
  method echoln vn str = if vn<=n then print_endline str

  (* modules *)    
  method moduleDef m rs = self#moduleDef_post m
  method moduleDef_val m = self#moduleDef_pre m 
  method moduleDef_pre m = self#echoln 2 ("\n[CSIZE_MODULE_START] csize pass in Module: " ^ m#name)
  method moduleDef_post m = self#echoln 2 ("\n[CSIZE_MODULE_END] csize pass in Module: " ^ m#name)
    
  (* definition *)
  method definition v m d r = self#definition_post m d
  method definition_val v m d = self#definition_pre m d
  method definition_pre m d = _current_choice_pile <- []
  method definition_post m d = 
    let csize =
      match _current_choice_pile with
	|[] -> failwith "probleme pile csize1"
	|v::[] -> v
	|v::v2::l -> 
	   Printf.printf "\nCSIZE %d %d" v v2;
	   failwith "probleme pile csize2"
    in
    d#setCsize csize
    
  (* process *)
  method choice v m d p rs = self#choice_post m d p
  method choice_val v m d p = self#choice_pre m d p
  method choice_pre m d p =
    _current_choice_pile <- 0::_current_choice_pile
  method choice_post m d p = ()

  method branch v m d p index b g a q = self#branch_post m d p index b
  method branch_val v m d p index b =  self#branch_pre m d p index b
  method branch_pre m d p index b = ()
  method branch_post m d p index b = ()

  method call v m d p rs = self#call_post m d p
  method call_val v m d p = self#call_pre m d p
  method call_pre m d p = ()
  method call_post m d p =
    let Def(def_called) = try
      m#lookupDef (p#defName)
    with Not_found -> failwith "undefined definition called...(calcul csize)"
    in (* should never happen *)
    if ((def_called#csize > 0) && (d#csize < def_called#csize)) then
      _current_choice_pile <- (def_called#csize)::_current_choice_pile
    else
      _current_choice_pile <- 0::_current_choice_pile
      
  method term v m d p = self#term_post m d p
  method term_val v m d p = self#term_pre m d p
  method term_pre m d p = ()
  method term_post m d p =  _current_choice_pile <- 0::_current_choice_pile
    
  (* action *)
  method outAction v m d p a r = self#outAction_post m d p a
  method outAction_val v m d p a = self#outAction_pre m d p a
  method outAction_pre m d p a = ()
  method outAction_post m d p a =
    match _current_choice_pile with
      |[] -> () (*devrait pas arriver*)
      |v::[] -> () (*devrait pas arriver*)
      |v1::v2::l -> if ((v1 + 1) > v2) then
	  _current_choice_pile <- (v1 + 1)::l
	else
	  _current_choice_pile <- v2::l
  method inAction v m d p a = self#inAction_post m d p a
  method inAction_val v m d p a = self#inAction_pre m d p a
  method inAction_pre m d p a = ()
  method inAction_post m d p a = 
    match _current_choice_pile with
      |[] -> () (*devrait pas arriver*)
      |v::[] -> () (*devrait pas arriver*)
      |v1::v2::l -> if ((v1 + 1) > v2) then
	  _current_choice_pile <- (v1 + 1)::l
	else
	  _current_choice_pile <- v2::l
  method tauAction v m d p a = self#tauAction_post m d p a
  method tauAction_val v m d p a = self#tauAction_pre m d p a
  method tauAction_pre m d p a = ()
  method tauAction_post m d p a = 
    match _current_choice_pile with
      |[] -> () (*devrait pas arriver*)
      |v::[] -> () (*devrait pas arriver*)
      |v1::v2::l -> if ((v1 + 1) > v2) then
	  _current_choice_pile <- (v1 + 0)::l
	else
	  _current_choice_pile <- v2::l

  method newAction v m d p a = self#newAction_post m d p a
  method newAction_val v m d p a = self#newAction_pre m d p a
  method newAction_pre m d p a = ()
  method newAction_post m d p a = 
    match _current_choice_pile with
      |[] -> () (*devrait pas arriver*)
      |v::[] -> () (*devrait pas arriver*)
      |v1::v2::l -> if ((v1 + 1) > v2) then
	  _current_choice_pile <- (v1 + 0)::l
	else
	  _current_choice_pile <- v2::l
  method spawnAction v m d p a rs = self#spawnAction_post m d p a
  method spawnAction_val v m d p a = self#spawnAction_pre m d p a
  method spawnAction_pre m d p a = ()
  method spawnAction_post m d p a = 
    match _current_choice_pile with
      |[] -> () (*devrait pas arriver*)
      |v::[] -> () (*devrait pas arriver*)
      |v1::v2::l -> if ((v1 + 1) > v2) then
	  _current_choice_pile <- (v1 + 0)::l
	else
	  _current_choice_pile <- v2::l
  method primAction v m d p a rs = self#primAction_post m d p a
  method primAction_val v m d p a = self#primAction_pre m d p a
  method primAction_pre m d p a = ()
  method primAction_post m d p a = 
    match _current_choice_pile with
      |[] -> () (*devrait pas arriver*)
      |v::[] -> () (*devrait pas arriver*)
      |v1::v2::l -> if ((v1 + 1) > v2) then
	  _current_choice_pile <- (v1 + 0)::l
	else
	  _current_choice_pile <- v2::l
  method letAction v m d p a r = self#letAction_post m d p a
  method letAction_val v m d p a = self#letAction_pre m d p a
  method letAction_pre m d p a = ()
  method letAction_post m d p a = 
    match _current_choice_pile with
      |[] -> () (*devrait pas arriver*)
      |v::[] -> () (*devrait pas arriver*)
      |v1::v2::l -> if ((v1 + 1) > v2) then
	  _current_choice_pile <- (v1 + 0)::l
	else
	  _current_choice_pile <- v2::l
    
  (* value *)
  method trueValue w m d p t v = self#trueValue_post m d p t v
  method trueValue_val w m d p t v = self#trueValue_pre m d p t v
  method trueValue_pre m d p t v = ()
  method trueValue_post m d p t v = ()
  method falseValue w m d p t v = self#falseValue_post m d p t v
  method falseValue_val w m d p t v = self#falseValue_pre m d p t v
  method falseValue_pre m d p t v = ()
  method falseValue_post m d p t v = ()
  method intValue w m d p t v = self#intValue_post m d p t v
  method intValue_val w m d p t v = self#intValue_pre m d p t v
  method intValue_pre m d p t v = ()
  method intValue_post m d p t v = ()
  method stringValue w m d p t v = self#stringValue_post m d p t v
  method stringValue_val w m d p t v = self#stringValue_pre m d p t v
  method stringValue_pre m d p t v = ()
  method stringValue_post m d p t v = ()
  method tupleValue w m d p t v vs = self#tupleValue_post m d p t v
  method tupleValue_val w m d p t v = self#tupleValue_pre m d p t v
  method tupleValue_pre m d p t v = ()
  method tupleValue_post m d p t v = ()
  method varValue w m d p t v = self#varValue_post m d p t v
  method varValue_val w m d p t v = self#varValue_pre m d p t v
  method varValue_pre m d p t v = ()
  method varValue_post m d p t v = ()
  method primValue w m d p t v vs = self#primValue_post m d p t v
  method primValue_val w m d p t v = self#primValue_pre m d p t v
  method primValue_pre m d p t v = ()
  method primValue_post m d p t v = ()
end


(** Computing passes *)

(* Debuggage de esize et csize *)
let print_sizes m nbpass =
  print_string ("\n passe : " ^ (string_of_int nbpass));
  let defs = List.map (fun (Def (def)) -> def) m#definitions in
  List.iter (fun def -> print_string (
	       "\n def: " ^ (def#name) ^ 
		 " esize : " ^ (string_of_int (def#esize)) ^ 
		 " csize : " ^ (string_of_int (def#csize)))) defs

(* If a def called has a higher value for esize or csize,
   the calling def must take this value. Thus, we must find a fixpoint for the sizes
   of each definition first. *)

(* In order to reduce the number of passes, we could order the defs in module in a 
   topological order -> then if there is no recursive call, one pass is enough. 
   If there are recursive calls two passes are enough.
   The defs are currently in a Hashtbl, so ... ? *)

let fixpoint_sizes m esize_pass csize_pass =
  let nb_pass = ref 0 in
  let rec fix_rec esizes csizes continue =
    if continue then (
      incr nb_pass;
      ignore (ASTUtils.module_fold m (ASTUtils.fold_seq csize_pass esize_pass));
      let Module(m') = m in
      print_sizes m' !nb_pass;
      let defs = List.map (fun (Def (def)) -> def) m'#definitions in
      let (esizes', csizes') = 
	List.fold_left ( fun (e, c) def -> (e + (def#esize), c + (def#csize))) 
	  (0,0) defs in
      Printf.printf "\n INFO : esize = %d, csize = %d, esize' = %d, csize' = %d" esizes csizes esizes' csizes';
      let continue = not((esizes = esizes') && (csizes = csizes')) in
      fix_rec esizes' csizes' continue
      (* the defs don't changes, and neither do the sizes*)
    )
    else
      ()
  in
  let Module(m') = m in
  let defs = List.map (fun (Def (def)) -> def) m'#definitions in
  let (esizes, csizes) = 
    List.fold_left ( fun (e, c) def -> (e+ (def#esize), c + (def#csize))) (0,0) defs in
  fix_rec esizes csizes true


let first_pass m verbosity = 
  let esize_pass = new env_compute_pass verbosity in
  let csize_pass = new csize_compute_pass 0 in
  (* fixpoint_sizes m esize_pass csize_pass; (\* recuperer esize? *\) *)
  ignore (ASTUtils.module_fold m (ASTUtils.fold_seq csize_pass esize_pass));
  ASTUtils.module_fold m
    (ASTUtils.fold_seq csize_pass (* utilité ? *)
       (ASTUtils.fold_compose
          (esize_pass) (* utilité ? *)
          (typing_pass 0)))




(* TESTS *)







(* class astCheck_pass (n:int) : [unit, unit] ASTUtils.fold_node =  *)
(* object(self) *)

(*   (\* config *\) *)
(*   method verbosity = n *)
(*   method echo vn str = if vn<=n then print_string str *)
(*   method echoln vn str = if vn<=n then print_endline str *)
(*     (\* module *\) *)
(*   method moduleDef_val m = self#echoln n("module_val : " ^ m#toString) *)
(*   method moduleDef m _ = self#echoln n ("module : " ^ m#toString ) *)
(*     (\* definitions *\) *)
(*   method definition_val _ m d = self#echoln n ("definition_val : " ^ m#toString ^ d#toString) *)
(*   method definition _ m d _ = self#echoln n ("definition : " ^ m#toString ^ d#toString) *)
(*     (\* processes *\) *)
(*   method choice_val _ m d p = self#echoln n ("choice_val :" ^ m#toString ^ d#toString ^ p#toString) *)
(*   method choice _ m d p v = self#echoln n ("choice :" ^ m#toString ^ d#toString ^ p#toString) *)
(*   method branch_val _ (m:module_type) (d:definition_type) (p:process choice_process_type) (i:int) (b:process prefix_process_type) =  *)
(*     match b#action with *)
(*       | Input a -> *)
(* 	  (match (d#lookupEnv a#channel) with *)
(* 	     | None -> self#echoln n ("branch_val : inputnonec " ^ m#toString ^ d#toString ^ p#toString ^ b#toString ^ a#toString) *)
(* 	     | Some n -> self#echoln n ("branch_val : inputc " ^ m#toString ^ d#toString ^ p#toString ^ b#toString ^ a#toString)); *)
(* 	  (match (d#lookupEnv a#variable) with *)
(* 	     | None -> self#echoln n ("branch_val : inputnonev " ^ m#toString ^ d#toString ^ p#toString ^ b#toString ^ a#toString) *)
(* 	     | Some n -> self#echoln n ("branch_val : inputv " ^ m#toString ^ d#toString ^ p#toString ^ b#toString ^ a#toString)) *)
(*       | Output a -> *)
(* 	  (match (d#lookupEnv a#channel) with *)
(* 	     | None -> self#echoln n ("branch_val : outputnone " ^ m#toString ^ d#toString ^ p#toString ^ b#toString ^ a#toString) *)
(* 	     | Some n -> self#echoln n ("branch_val : output " ^ m#toString ^ d#toString ^ p#toString ^ b#toString ^ a#toString)) *)
(*       | New a ->   *)
(* 	  (match (d#lookupEnv a#variable) with *)
(* 	     | None -> self#echoln n ("branch_val : newnone " ^ m#toString ^ d#toString ^ p#toString ^ b#toString ^ a#toString) *)
(* 	     | Some n -> self#echoln n ("branch_val : new " ^ m#toString ^ d#toString ^ p#toString ^ b#toString ^ a#toString))	 *)
(*       | Let a ->  (match (d#lookupEnv a#variable) with *)
(* 		     | None -> self#echoln n ("branch_val : letnone " ^ m#toString ^ d#toString ^ p#toString ^ b#toString ^ a#toString) *)
(* 		     | Some n -> self#echoln n ("branch_val : let " ^ m#toString ^ d#toString ^ p#toString ^ b#toString ^ a#toString)) *)
(*       | _ -> self#echoln n ("branch_val : autre " ^ m#toString ^ d#toString ^ p#toString ^ b#toString) *)
(*   method branch _ m d p i b s1 s2 s3 = self#echoln n ("branch : "^ m#toString ^ d#toString ^ p#toString ^ b#toString) *)
(*   method call_val _ m d p = self#echoln n ("") *)
(*   method call _ m d p _ = self#echoln n ("") *)
(*   method term_val _ m d p = self#echoln n ("") *)
(*   method term _ m d p = self#echoln n ("") *)
(*     (\* actions *\) *)
(*   method outAction_val _ m d p a = self#echoln n ("") *)
(*   method outAction _ m d p a r = self#echoln n ("") *)
(*   method inAction_val _ m d p a = self#echoln n ("") *)
(*   method inAction _ m d p a =  *)
(*     match (d#lookupEnv a#variable) with *)
(*       | None -> self#echoln n ("") *)
(*       | Some _ -> self#echoln n ("") *)
(*   method tauAction_val _ m d p a = self#echoln n ("") *)
(*   method tauAction _ m d p a = self#echoln n ("") *)
(*   method newAction_val _ m d p a = self#echoln n ("") *)
(*   method newAction _ m d p a =  *)
(*     match (d#lookupEnv a#variable) with *)
(*       | None -> self#echoln n ("") *)
(*       | Some _ -> self#echoln n ("") *)
(*   method spawnAction_val _ m d p a = self#echoln n ("") *)
(*   method spawnAction _ m d p a rs = self#echoln n ("") *)
(*   method primAction_val _ m d p a = self#echoln n ("") *)
(*   method primAction _ m d p a rs = self#echoln n ("") *)
(*   method letAction_val _ m d p a = self#echoln n ("") *)
(*   method letAction _ m d p a r =  *)
(*     match (d#lookupEnv a#variable) with *)
(*       | None -> self#echoln n ("") *)
(*       | Some _ -> self#echoln n ("") *)
(* 	  (\* value *\) *)
(*   method trueValue_val _ m d p t v = self#echoln n ("") *)
(*   method trueValue _ m d p t v = self#echoln n ("") *)
(*   method falseValue_val _ m d p t v = self#echoln n ("") *)
(*   method falseValue _ m d p t v = self#echoln n ("") *)
(*   method intValue_val _ m d p t v = self#echoln n ("") *)
(*   method intValue _ m d p t v = self#echoln n ("") *)
(*   method stringValue_val _ m d p t v = self#echoln n ("") *)
(*   method stringValue _ m d p t v = self#echoln n ("") *)
(*   method tupleValue_val _ m d p t v = self#echoln n ("") *)
(*   method tupleValue _ m d p t v rs = self#echoln n ("") *)
(*   method varValue_val _ m d p t v =  *)
(*     match (d#lookupEnv v#name) with *)
(*       | None -> self#echoln n ("") *)
(*       | Some n -> self#echoln n ("") *)
(*   method varValue _ m d p t v = self#echoln n ("") *)
(*   method primValue_val _ m d p t v = self#echoln n ("") *)
(*   method primValue _ m d p t v rs = self#echoln n ("") *)
(* end *)







(* il faudrait faire e_size et c_size en parallele ici avec un fold_seq et utiliser une structure pour mémoriser les defs *)
(* dans les methodes de call on regarde dans la structure si *)

(* let pass_esize m verbosity =  *)
(*   let rec pass_recursive e_c_p = *)
(*     ASTUtils.fold_module m (e_c_p verbosity)  *)
(*   in  *)
(*   pass_recursive (new env_compute_pass verbosity) *)




(* let print_pass m verbosity =  *)
(*   ASTUtils.fold_module m *)
(*     (new astCheck_pass 2)  *)

(* let compute_pass m verbosity =  *)
(*   print_pass m verbosity *)


(* let first_pass m verbosity =   *)
(*   ASTUtils.fold_module m  *)
(*     (ASTUtils.fold_seq (new csize_compute_pass verbosity)   *)
(*        (ASTUtils.fold_compose  *)
(*           (new env_compute_pass verbosity)  *)
(*           (typing_pass verbosity))) *)
