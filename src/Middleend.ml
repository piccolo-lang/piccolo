(* module Middleend
   ---------------

   Middleend passes

*)
(** This module defines the middle end part of the compiler. Four passes are computed :
    - esize -> compute the maximum environment size
    - csize -> compute the maximum commitment size
    - channel_max -> compute the maximum number of channels
    - choice_max -> compute the maximum number of branches in each choice process and return the maximum value
*)

open Utils;;
open Syntax;;
open Typing;;

(** Representation of a [string list,int] fold_node. 
    Compute esize and attribute index to each variable. *)
class env_size_pass (n:int) : [string list, int] ASTUtils.fold_node =
  let lookup env v =
    let rec aux env n = match env with
      | [] -> None
      | w::env' -> if v=w then Some n else aux env' (n+1)
    in aux env 0
  in
object(self)
  
  (* This list contains the new variables we might run into while walking through the tree. It doesn't contain the variables given as parameters of definitions, only the ones met in Let, In, and New. *)
  val mutable newVar = []
  (* This list contains the esizes of the called definitions. *)
  val mutable calledDefs = []

  (* config *)
  method verbosity = n
  method echo vn str = if vn<=n then print_string str
  method echoln vn str = if vn<=n then print_endline str
    
  (* module *)
  method moduleDef_val m = []
  method moduleDef m esizes =
    self#echoln 3 ("\n[ESIZE_MODULE] env pass finished in Module: " ^ m#name);
    list_max esizes 

  (* definitions *)
  method definition_val _ m (d:definition_type) =
    self#echoln 3 ("\n[ESIZE_DEF] Start : " ^ (d#name) ^ " env start : " );
    calledDefs <- [];
    newVar <- [];
    let (env, _) = List.split d#params in (* the start env contains only the parameters of the def *)
    env
  method definition _ m d esize =
    (* Retrieve the maximum esize of the defs called *)
    let max_called = 
      List.fold_left (fun max esize_def -> 
	if esize_def > max then esize_def else max)
	0 calledDefs in
    let esize' = d#arity + esize in
    let res = if (max_called > esize')
      then max_called
      else esize'
    in
    self#echoln 3 ("\n[ESIZE_DEF] env pass finished in Definition: " ^
		      d#name ^ " ==> computed env size = " ^ (string_of_int res));
    d#setEsize res;
    res

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
	     | None -> 
		 (match (lookup d#env a#variable) with
		    | None -> (a#setVariableIndex (List.length d#env)) ;
			d#extendEnv a#variable
		    | Some _ -> ()
		 );
		 newVar <- (a#variable)::newVar; 
		 (* we add a in the list of new variables *)
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
		 (match (lookup d#env a#variable) with
		    | None -> d#extendEnv a#variable;
		    | Some _ -> ()
		 );
		 newVar <- (a#variable)::newVar; 
		 (* we add a in the list of new variables *)
		 env @ [a#variable]
	     | Some n -> a#setVariableIndex n ; env)	    
      | Let a ->  (match (lookup env a#variable) with
		     | None -> a#setVariableIndex (List.length env) ;
		       newVar <- (a#variable)::newVar;
		       (match (lookup d#env a#variable) with
			  | None -> d#extendEnv a#variable;
			  | Some _ -> ()
		       );
		       env @ [a#variable]
		     | Some n -> a#setVariableIndex n ; env)
      | _ -> env
  method branch env m d p i b s1 s2 s3 = 
    s1+s2+s3
  method call_val env m d p = env
  method call env m d p _ = 
    let Def(def_called) = try
      m#lookupDef (p#defName)
    with Not_found -> failwith "undefined definition called...(calcul esize)"
      (* sould never happen *)
    in
    let esize_called = def_called#esize in
    calledDefs <- esize_called::calledDefs;
    0

  method term_val env m d p = ()
  method term env m d p = 0
  
  (* actions *)
  method outAction_val env m d p a = env
  method outAction env m d p a r = 0
  method inAction_val env m d p a = ()
  method inAction env m d p a = 
    if (List.mem a#variable newVar) then ( 
      newVar <- List.filter (fun v -> v <> a#variable) newVar;
      1)
    else (
      0) 
  method tauAction_val env m d p a =  ()
  method tauAction env m d p a = 0
  method newAction_val env m d p a = ()
  method newAction env m d p a = 
    if (List.mem a#variable newVar) then ( 
      newVar <- List.filter (fun v -> v <> a#variable) newVar;
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
      newVar <- List.filter (fun v -> v <> a#variable) newVar;
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


(** Pass computing the max number of commitments. The value is stored in each definitions. *)
class commitment_size_pass (n:int) : [unit, int] ASTUtils.fold_node = 
object(self)
  
  (* This list contains the new variables we might run into while walking through the tree. It doesn't contain the variables given as parameters of definitions, only the ones met in Let, In, and New. *)
  val mutable calledDefs = []
    
  (* config *)
  method verbosity = n
  method echo vn str = if vn<=n then print_string str
  method echoln vn str = if vn<=n then print_endline str
    
  (* module *)
  method moduleDef_val m = ()
  method moduleDef m nbCommits =
    list_max nbCommits

  (* definitions *)
  method definition_val _ m (d:definition_type) =
    calledDefs <- [];
    self#echoln 3 ("\n[Commits_pass_DEF] Start : " ^ (d#name));
    ()
  method definition _ m d nbcommits=
    let max_of_called_defs = 
      List.fold_left (fun acc nbcommits_def -> max nbcommits_def acc) 0 calledDefs in
    let res = max max_of_called_defs nbcommits in 
    self#echoln 3 ("\n[Commits_pass_DEF] pass finished in Definition: " ^
		     d#name ^ " => computed csize = " ^ (string_of_int res)) ;
    d#setCsize res;
    res

  (* processes *)
  method choice_val _ m d p = ()
  method choice _ m d p csizes = list_max csizes

  method branch_val _ (m:module_type) (d:definition_type) (p:process choice_process_type) (i:int) (b:process prefix_process_type) = ()
  method branch _ m d p i b s1 s2 s3 = 
    s1+s2+s3

  method call_val _ m d p = ()
  method call _ m d p _ = 
    let Def(def_called) = try
      m#lookupDef (p#defName)
    with Not_found -> failwith "undefined definition called...(calcul csize)"
      (* sould never happen *)
    in
    let csize_called = def_called#csize in
    calledDefs <- csize_called::calledDefs;
    0
  method term_val _ m d p = ()
  method term _ m d p = 0

  (* actions *)
  method outAction_val _ m d p a = ()
  method outAction _ m d p a r = 1
  method inAction_val _ m d p a = ()
  method inAction _ m d p a = 1
  method tauAction_val _ m d p a =  ()
  method tauAction _ m d p a = 0
  method newAction_val _ m d p a = ()
  method newAction _ m d p a = 0
  method spawnAction_val _ m d p a = ()
  method spawnAction _ m d p a rs = 0
  method primAction_val _ m d p a = ()
  method primAction _ m d p a rs = 0
  method letAction_val _ m d p a = ()
  method letAction _ m d p a r = 0

  (* value *)
  method trueValue_val _ m d p t v = ()
  method trueValue _ m d p t v = 0
  method falseValue_val _ m d p t v = ()
  method falseValue _ m d p t v = 0
  method intValue_val _ m d p t v = ()
  method intValue _ m d p t v = 0
  method stringValue_val _ m d p t v = ()
  method stringValue _ m d p t v = 0
  method tupleValue_val _ m d p t v = ()
  method tupleValue _ m d p t v rs = 0
  method varValue_val _ m d p t v = ()
  method varValue _ m d p t v = 0
  method primValue_val _ m d p t v = ()
  method primValue _ m d p t v rs = 0
end





(** Pass computing the max number of channels. The value is stored in each definitions. *) 
class channel_pass (n:int) : [string list, int] ASTUtils.fold_node = 
  let lookup env v =
    let rec aux env n = match env with
      | [] -> None
      | w::env' -> if v=w then Some n else aux env' (n+1)
    in aux env 0
  in
object(self)
  val mutable calledDefs = []
  val mutable newChan = []
  (* config *)
  method verbosity = n
  method echo vn str = if vn<=n then print_string str
  method echoln vn str = if vn<=n then print_endline str
  (* module *)
  method moduleDef_val m = []
  method moduleDef m nbchans =
    list_max nbchans 
  (* definitions *)
  method definition_val _ m (d:definition_type) =
    calledDefs <- [];
    newChan <- [];
    let chans = List.fold_left (fun acc (name, typ) -> 
				  match typ with
				    | Types.TChan(a) -> name::acc
				    | _ -> acc 
    ) [] d#params in
    chans
  method definition _ m d nbchan =
    let nbchanTotal = (List.length 
			(List.fold_left (fun acc (name, typ) -> 
			  match typ with
			    |Types.TChan (a) -> name::acc
			    |_ -> acc) [] d#params) 
		      + nbchan) in
    let max_called = 
      List.fold_left (fun max nbchan_def -> 
	if nbchan_def > max then nbchan_def else max) 
	0 calledDefs in
    let res = if (max_called > nbchanTotal) 
      then max_called
      else nbchanTotal
    in
    d#setNbChannels res;
    res
  (* processes *)
  method choice_val chan_env m d p = chan_env
  method choice chan_env m d p nbchans = list_max nbchans
  method branch_val chan_env (m:module_type) (d:definition_type) (p:process choice_process_type) (i:int) (b:process prefix_process_type) = 
    match b#action with
      | New a ->
	  (match (lookup chan_env a#variable) with
	     | None -> 
		 newChan <- a#variable::newChan;
		 chan_env @ [a#variable]
	     | Some n -> chan_env)
      | _ -> chan_env	    
  method branch chan_env m d p i b s1 s2 s3 = 
    s1+s2+s3
  method call_val chan_env m d p = chan_env
  method call chan_env m d p _ = 
    let Def(def_called) = try
      m#lookupDef (p#defName)
    with Not_found -> failwith "undefined definition called...(calcul nbchan)"
      (* sould never happen *)
    in
    let nbchan_called = def_called#nbChannels in
    calledDefs <- nbchan_called::calledDefs;
    0
  method term_val chan_env m d p = ()
  method term chan_env m d p = 0
  (* actions *)
  method outAction_val chan_env m d p a = chan_env
  method outAction chan_env m d p a r = 0
  method inAction_val chan_env m d p a = ()
  method inAction chan_env m d p a = 0
  method tauAction_val chan_env m d p a =  ()
  method tauAction chan_env m d p a = 0
  method newAction_val chan_env m d p a = ()
  method newAction chan_env m d p a = 
    if (List.mem a#variable newChan) then ( 
      newChan <- List.filter (fun v -> v <> a#variable) newChan;
      1 )
    else (
      0)
  method spawnAction_val chan_env m d p a = chan_env
  method spawnAction chan_env m d p a rs = 0
  method primAction_val chan_env m d p a = chan_env
  method primAction chan_env m d p a rs = 0
  method letAction_val chan_env m d p a = chan_env
  method letAction chan_env m d p a r = 0
  (* value *)
  method trueValue_val chan_env m d p t v = ()
  method trueValue chan_env m d p t v = 0
  method falseValue_val chan_env m d p t v = ()
  method falseValue chan_env m d p t v = 0
  method intValue_val chan_env m d p t v = ()
  method intValue chan_env m d p t v = 0
  method stringValue_val chan_env m d p t v = ()
  method stringValue chan_env m d p t v = 0
  method tupleValue_val chan_env m d p t v = chan_env
  method tupleValue chan_env m d p t v rs = 0
  method varValue_val chan_env m d p t v = ()
  method varValue chan_env m d p t v = 0
  method primValue_val chan_env m d p t v = chan_env
  method primValue chan_env m d p t v rs = 0
end


(** Pass computing the max number of branchs in a choice. The value is stored in each definitions. *) 
class choice_pass (n:int) : [unit, int] ASTUtils.fold_node = 
object(self)
  (* This list contains the new variables we might run into while walking through the tree. It doesn't contain the variables given as parameters of definitions, only the ones met in Let, In, and New. *)
  val mutable calledDefs = []
  (* Contains the maximum choice. *)
  val mutable currentMax = 0
  (* config *)
  method verbosity = n
  method echo vn str = if vn<=n then print_string str
  method echoln vn str = if vn<=n then print_endline str
  (* module *)
  method moduleDef_val m = ()
  method moduleDef m nbchoice =
    list_max nbchoice
  (* definitions *)
  method definition_val _ m (d:definition_type) =
    calledDefs <- [];
    currentMax <- 0;
    ()
  method definition _ m d nbchoice =
    let max_called = 
      List.fold_left (fun max nbchoice_def -> 
			if nbchoice_def > max then nbchoice_def else max) 
	0 calledDefs in
    let res = if max_called > currentMax then max_called else currentMax in
    d#setNbChoiceMax res;
    res
  (* processes *)
  method choice_val _ m d p = ()
  method choice _ m d p nbchoice = 
    if (List.length nbchoice > currentMax) then (
      currentMax <- List.length nbchoice;
      List.length nbchoice)
    else currentMax
  method branch_val _ (m:module_type) (d:definition_type) (p:process choice_process_type) (i:int) (b:process prefix_process_type) =
    ()
  method branch _ m d p i b s1 s2 s3 = 
    1
  method call_val _ m d p = ()
  method call _ m d p _ =  
    let Def(def_called) = try
      m#lookupDef (p#defName)
    with Not_found -> failwith "undefined definition called...(calcul nbchan)"
      (* sould never happen *)
    in
    let nbchoice_called = def_called#nbChoiceMax in
    calledDefs <- nbchoice_called::calledDefs;
    0
  method term_val _ m d p = () 
  method term _ m d p = 0
  (* actions *)
  method outAction_val _ m d p a = ()
  method outAction _ m d p a r = 0
  method inAction_val _ m d p a = ()
  method inAction _ m d p a = 0
  method tauAction_val _ m d p a =  ()
  method tauAction _ m d p a = 0
  method newAction_val _ m d p a = ()
  method newAction _ m d p a = 0
  method spawnAction_val _ m d p a = ()
  method spawnAction _ m d p a rs = 0
  method primAction_val _ m d p a = ()
  method primAction _ m d p a rs = 0
  method letAction_val _ m d p a = ()
  method letAction _ m d p a r = 0
  (* value *)
  method trueValue_val _ m d p t v = ()
  method trueValue _ m d p t v = 0
  method falseValue_val _ m d p t v = ()
  method falseValue _ m d p t v = 0
  method intValue_val _ m d p t v = ()
  method intValue _ m d p t v = 0
  method stringValue_val _ m d p t v = ()
  method stringValue _ m d p t v = 0
  method tupleValue_val _ m d p t v = ()
  method tupleValue _ m d p t v rs = 0
  method varValue_val _ m d p t v = ()
  method varValue _ m d p t v = 0
  method primValue_val _ m d p t v = ()
  method primValue _ m d p t v rs = 0
end



(** Computing passes *)

(* Debug tool *)
(** Print the values of esize, csize, nbchannelmax, nbchoicemax for each definition *)
let print_values m =
  let defs = List.map (fun (Def (def)) -> def) m#definitions in
  List.iter (fun def -> print_string (
	       "\n def: " ^ (def#name) ^ 
		 ", esize : " ^ (string_of_int (def#esize)) ^ 
		 ", csize : " ^ (string_of_int (def#csize)) ^ 
		 ", nbchannels : " ^ (string_of_int (def#nbChannels)) ^
		 ", nbchoices : " ^ (string_of_int (def#nbChoiceMax)))) defs;
    print_string ("\n\n")

(* If a def called has a higher value for esize or csize,
   the calling def must take this value. Thus, we must find a fixpoint for the sizes
   of each definition first. *)

(* In order to reduce the number of passes, we could order the defs in module in a 
   topological order -> then if there is no recursive call, one pass is enough. 
   If there are recursive calls two passes are enough.
   The defs are currently in a Hashtbl, so ... ? *)

(* fixpoint : if definition are called at the end of a definition, we must check if the definition
   has a higher value for esize, csize, nbchannel and nbchoicemax. 
   Thus, we compute the passes until the esize, csize, channel and choice compute the same value twice in a row. *)

(** Compute the fixpoint of esize, csize, nbchannelmax, nbchoicemax for each definition *)
let fixpoint m esize_pass csize_pass channel_pass choice_pass verbosity =
  let nb_pass = ref 0 in
  let rec fix_rec esizes csizes channels choices continue =
    if continue then (
      incr nb_pass;
      ignore (ASTUtils.module_fold m 
		(ASTUtils.fold_compose channel_pass 
		   (ASTUtils.fold_compose choice_pass
		      (ASTUtils.fold_compose csize_pass esize_pass))));
      let Module(m') = m in
      if (verbosity > 4) then (
	print_string ("\n passe : " ^ (string_of_int !nb_pass));
	print_values m');
      let defs = List.map (fun (Def (def)) -> def) m'#definitions in
      let (esizes', csizes', channels', choices') = 
	List.fold_left (fun (e, c, chan, choice) def -> 
	  (e + def#esize, c + def#csize, chan + def#nbChannels , choice + def#nbChoiceMax))
	  (0, 0, 0, 0) defs in
      let continue = not((esizes = esizes') && (csizes = csizes') && 
	  (channels = channels') && (choices = choices')) in
      fix_rec esizes' csizes' channels' choices' continue
    )
    else
      ()
  in
  let Module(m') = m in
  let defs = List.map (fun (Def (def)) -> def) m'#definitions in
  let (esizes, csizes, channels, choices) = 
    List.fold_left ( fun (e, c, chan, choice) def -> 
      (e + def#esize, c + def#csize, chan + def#nbChannels, choice + def#nbChoiceMax)) 
      (0, 0, 0, 0) defs in
  fix_rec esizes csizes channels choices true

(** Compute middleend passes. *)
let compute_pass m verbosity =
  if verbosity >=1 then print_string "\nTyping pass start...\n";
  let errors = ASTUtils.module_fold m (typing_pass verbosity) in
  if verbosity >=1 then print_string "Typing pass ended!";
  let esize_pass = new env_size_pass verbosity in
  let csize_pass = new commitment_size_pass verbosity in
  let channel_pass = new channel_pass verbosity in
  let choice_pass = new choice_pass verbosity in
  if verbosity >=1 then print_string "\nMiddleend passes start...";
  fixpoint m esize_pass csize_pass channel_pass choice_pass verbosity;
  if verbosity >=1 then print_string "\nMiddleend passes ended!\n";
  if verbosity >= 2 then (
    let Module(m') = m in 
    print_values m');
  errors
