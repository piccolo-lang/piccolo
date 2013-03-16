(* module Typing
   -------------
   
   Type checking and (simple) inference

*)
(** this module executes a type checking pass used in MiddleEnd. *)

open Utils;;
open Types;;
open Syntax;;

(** type representing typeError *)
type typeError =
  | TypeError of (string * ast_type)
;;
      
(** string representing a typeError *)
let string_of_typeError = function
  | TypeError (msg, ast) -> "Error at "  ^ (ast#posString) ^ "\n  ==> " ^ msg
;;
      
(** type representing a list of typeErrors *)      
type typeErrors = typeError list;;
    
(** string representing a list of typeErrors *)
let string_of_typeErrors errs = string_of_collection "" "" "\n" string_of_typeError errs;;
  
(** representation of the type of a fold_node *)
type typingEnv = (valueType * ast_binder_type) SMap.t;;

(** string representing a typing_env *)
let print_typingEnv env =
  Printf.printf "env : [ ";
  SMap.iter (fun x (vt, _) -> Printf.printf "%s : %s -- " x (string_of_valueType vt)) env;
  Printf.printf "]\n"
;;

(** find a variable v from the corresponding environmment env *)
let lookup_env env v = 
  try 
    Some (SMap.find v env)
  with Not_found -> None
;;

(** find a definition name from a module, raise Not_found exception if not found *)
let lookup_def m n =
  try
    List.find (fun (Def d) -> d#name = n) m#definitions
  with Not_found -> failwith ("Unbound value " ^ n)
;;

(** Thread a typing env which is enriched with definitions and choice_process
  * return a list of errors wich is printed in level 2 verbosity
  *)
class typing_pass_node (n : int) : [typingEnv, typeErrors] ASTUtils.fold_node = object(self)
  (* level of verbosity for the output messages *)
  method verbosity = n
  
  method echo vn str = if vn <= n then print_string str
    
  method echoln vn str = if vn <= n then print_endline str

  (* value *)
  (* value true *)
  method trueValue_val (env : typingEnv) (m : module_type) (d : definition_type) (p : process_type) (t : Types.valueType) (v : bool const_value_type) : unit =
    self#echoln 4 "\n    < TYPING BOOLEAN > started";
    if(type_eq t TBool)then
      v#setType TBool
    else
      ()
	
  method trueValue (env : typingEnv) (m : module_type) (d : definition_type) (p : process_type) (t : Types.valueType) (v : bool const_value_type) : typeErrors =
    self#echoln 4 "\n    < TYPING CONSTANT TRUE > finished";
    let errs =
      match t with
	| TBool -> []
	| _ -> [TypeError ("Type Error: This expression has type " ^ (string_of_valueType t) ^ " but an expression was expected of type bool", (v :> ast_type))]
    in
      errs
	
  (* value false *)
  method falseValue_val (env : typingEnv) (m : module_type) (d : definition_type) (p : process_type) (t : Types.valueType) (v : bool const_value_type) : unit =
    self#echoln 4 "\n    < TYPING BOOLEAN > started";
    if(type_eq t TBool)then
      v#setType TBool
    else
      ()
	
  method falseValue (env : typingEnv) (m : module_type) (d : definition_type) (p : process_type) (t : Types.valueType) (v : bool const_value_type) : typeErrors =
    self#echoln 4 "\n    < TYPING CONSTANT FALSE > finished";
    let errs =
      match t with
	| TBool -> []
	| _ ->  [TypeError ("Type Error: This expression has type " ^ (string_of_valueType t) ^ " but an expression was expected of type bool", (v :> ast_type))]
    in
      errs
	
  (* value int *)
  method intValue_val (env : typingEnv) (m : module_type) (d : definition_type) (p : process_type) (t : Types.valueType) (v : int const_value_type) : unit = 
    self#echoln 4 "\n    < TYPING INTEGER > started";
    if(type_eq t TInt)then
      v#setType TInt
    else
      ()
  
  method intValue (env : typingEnv) (m : module_type) (d : definition_type) (p : process_type) (t : Types.valueType) (v : int const_value_type) : typeErrors =
    self#echoln 4 "\n    < TYPING CONSTANT INT > finished";
    let errs =
      match t with
	| TInt -> []
	| _ -> [TypeError ("Type Error: This expression has type " ^ (string_of_valueType t) ^ " but an expression was expected of type int", (v :> ast_type))]
    in
      errs
	
  (* value string *)
  method stringValue_val (env : typingEnv) (m : module_type) (d : definition_type) (p : process_type) (t : Types.valueType) (v : string const_value_type) : unit =
    self#echoln 4 "\n    < TYPING STRING > started";
    if(type_eq t TString)then
      v#setType TString
    else
      ()
	
  method stringValue (env : typingEnv) (m : module_type) (d : definition_type) (p : process_type) (t : Types.valueType) (v : string const_value_type) : typeErrors =
    self#echoln 4 "\n    < TYPING CONSTANT STRING > finished";
    let errs =
      match t with
	| TString -> []
	| _ -> [TypeError ("Type Error: This expression has type " ^ (string_of_valueType t) ^ " but an expression was expected of type string", (v :> ast_type))]
    in
      errs
	
  (* value tuple *)
  method tupleValue_val (env : typingEnv) (m : module_type) (d : definition_type) (p : process_type) (t : Types.valueType) (v : value tuple_value_type) : typingEnv =
    self#echoln 3 "\n    < TYPING TUPLE > started";
    let rec funct var_list type_list =
      match var_list with
	| [] -> []
	| head::tail ->
	    (match head with
	       | VVar (head) ->
		   (match lookup_env env head#name with
		      | None -> failwith ("Error: Unbound value " ^ head#name)
		      | Some (ty, _) -> ty::(funct tail (List.tl type_list)))
	       | VPrim (head) ->
		   let prim = PrimitiveUtils.get_value_type head#moduleName head#primName in 
		     prim#return::(funct tail (List.tl type_list))
	       | _ -> (List.hd type_list)::(funct tail (List.tl type_list)))
    in
      v#setTypes (funct v#elements v#types);
      env
	
  method tupleValue (env : typingEnv) (m : module_type) (d : definition_type) (p : process_type) (t : Types.valueType) (v : value tuple_value_type) (errs : typeErrors list) : typeErrors =
    self#echoln 3 "\n    < TYPING TUPLE > finished";
    let tuple_errs =
      match t with
	| TTuple (t') -> []
	| _ -> [TypeError ("Type Error: This expression has type " ^ (string_of_valueType t) ^ " but an expression was expected of type " ^ (string_of_valueType v#ofType), (v :> ast_type))]
    in
      tuple_errs@(List.concat errs)
	
  (* value variable *) 
  method varValue_val (env : typingEnv) (m : module_type) (d : definition_type) (p : process_type) (t : Types.valueType) (v : variable_type) : unit =
    self#echoln 2 "\n    < TYPING VARIABLE >";
    ()
      
  method varValue (env : typingEnv) (m : module_type) (d : definition_type) (p : process_type) (t : Types.valueType) (v : variable_type) : typeErrors =
    match lookup_env env v#name with
      | Some (TUnknown, binder) ->
	  v#setType TUnknown;
	  v#setBinder binder;
	  self#echoln 5 (Printf.sprintf "ERROR setted to %s \n" v#toString);      
	  [TypeError (("Unknown type for " ^ v#name), (v :> ast_type))]
      | Some (t', binder) ->
	  v#setType t';
	  v#setBinder binder;
	  self#echoln 5 (Printf.sprintf "=> variable %s setted to %s" v#name v#toString);
	  (if(type_eq t' t)then
	     []
	   else
	     [TypeError (("Type Error: " ^ v#name ^ " This expression has type " ^  (string_of_valueType t') ^ " but an expression was expected of type " ^ (string_of_valueType t)), (v :> ast_type))])
      | None -> [TypeError (("Unbound variable " ^ v#name), (v :> ast_type))]
	  
  (* value primitive *)
  method primValue_val (env : typingEnv) (m : module_type) (d : definition_type) (p : process_type) (t : Types.valueType) (v : value prim_value_type) : typingEnv = 
    self#echoln 3 "\n    < TYPING PRIMITIVE > started";
    let prim = PrimitiveUtils.get_value_type v#moduleName v#primName in 
      v#setArgTypes prim#params;
      v#setReturnType prim#return;
      env
	
  method primValue (env : typingEnv) (m : module_type) (d : definition_type) (p : process_type) (t: Types.valueType) (v : value prim_value_type) (errs: typeErrors list) : typeErrors =
    self#echoln 3 "\n    < TYPING PRIMITIVE > finished";
    let prim = PrimitiveUtils.get_value_type v#moduleName v#primName in 
      (if(not(prim#arity = v#arity))then
	 failwith ("Arity Error: This primitive has arity of size " ^  (string_of_int v#arity) ^ " but an the primitive should have arity of size" ^ (string_of_int prim#arity))
       else
	 ());
      let errl =
	List.fold_left2
	  (fun l t1 t2 ->
	     if(type_eq t1 t2)then
	       l
	     else
	       TypeError ("Mismatch PRIMVALUE § § § : ", (v :> ast_type))::l)
	  [] prim#params v#argTypes (* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*)
      in
	errl@(List.concat errs)
	  
  (* action *)
  (* out actions *)
  method outAction_val (env : typingEnv) (m : module_type) (d : definition_type) (p : process prefix_process_type) (a : out_action_type) : typingEnv =
    self#echoln 2 "\n    < TYPING OUT ACTION > started";
    env
      
  method outAction (env : typingEnv) (m : module_type) (d : definition_type) (p : process prefix_process_type) (a : out_action_type) (errs : typeErrors) : typeErrors =
    self#echoln 2 "\n    < TYPING OUT ACTION > finished";
    (match lookup_env env a#channel with
       | None -> [TypeError ("Unbound channel " ^ a#channel, (a :> ast_type))]
       | Some (chType, _) ->
	   a#setChannelType chType;
	   try 
	     let valType = 
	       match a#value with 
		 | VVar (v) ->
		     (match lookup_env env v#name with
			| Some (ty, _) -> a#setValueType ty; ty
			| None -> failwith v#name)
		 | VTuple (t) -> print_endline t#toString; a#setValueType t#ofType; t#ofType
		 | VPrim (p) -> print_endline "prim"; a#setValueType p#returnType; p#returnType
		 | _ -> a#valueType
	     in
	       (match (chType, valType) with
		  | (TChan (t1) as chan, t2) ->
		      if(type_eq t1 t2)then
			[]
		      else
			[TypeError (("Type Error: This expression has type " ^  (string_of_valueType chan) ^ " but an expression was expected of type " ^ (string_of_valueType (TChan (t2))), (a :> ast_type)))]
		  | (_ as t, t2) -> [TypeError (("Type Error: This expression has type " ^  (string_of_valueType t) ^ " but an expression was expected of type " ^ (string_of_valueType (TChan (t2))), (a :> ast_type)))])
	   with Failure var_name -> [TypeError ("Unbound variable " ^ var_name, (a :> ast_type))]
    )@errs
      
  (* in action *)
  method inAction_val (env : typingEnv) (m : module_type) (d : definition_type) (p : process prefix_process_type) (a : in_action_type) : unit =
    self#echoln 2 "\n    < TYPING INPUT ACTION > started";
    match d#fetchBinderType a#channel with 
      | Some (TChan (vt)) -> 
	  (a#setChannelBinder (d :> ast_binder_type);
	   a#setVariableType vt; 
	   self#echoln 5 (Printf.sprintf "=> input variable %s setted to type %s" a#variable (string_of_valueType vt)));
      | Some _ | None -> ()
	  
  method inAction (env : typingEnv) (m : module_type) (d : definition_type) (p : process prefix_process_type) (a : in_action_type) : typeErrors =
    self#echoln 2 "\n    < TYPING INPUT ACTION > finished";
    match d#fetchBinderType a#channel with 
      |	None -> [TypeError (("Unbound channel " ^ a#channel), (a :> ast_type))]
      | Some (TChan (vt)) -> []
      | Some (_ as t) -> [TypeError (("Type Error: This expression has type " ^ (string_of_valueType t) ^ " but an expression was expected of type " ^ (string_of_valueType a#channelType), (a :> ast_type)))]
	  
  (* tau action *)
  method tauAction_val (env : typingEnv) (m : module_type) (d : definition_type) (p : process prefix_process_type) (a : tau_action_type) : unit =
    self#echoln 5 "\n    < TYPING TAU ACTION > started";
    ()
      
  method tauAction (env : typingEnv) (m : module_type) (d : definition_type) (p : process prefix_process_type) (a : tau_action_type) : typeErrors =
    self#echoln 5 "\n    < TYPING TAU ACTION > finished";
    []
      
  (* new action /!\ *)
  method newAction_val (env : typingEnv) (m : module_type) (d : definition_type) (p : process prefix_process_type) (a : new_action_type) : unit =
    self#echoln 2 "\n    < TYPING NEW ACTION > started";
    ()
      
  method newAction (env : typingEnv) (m : module_type) (d : definition_type) (p : process prefix_process_type) (a : new_action_type) : typeErrors =
    self#echoln 2 "\n    < TYPING NEW ACTION > finished";
    match a#variableType with
      | TChan (t) -> []
      | _ -> [TypeError ("Type Error: " ^ a#variable ^ "This expression has type " ^ (string_of_valueType a#variableType) ^ " but an expression was expected of type channel", (a :> ast_type))]
	  
  (* spawn action *)
  method spawnAction_val (env : typingEnv) (m : module_type) (d : definition_type) (p : process prefix_process_type) (a : spawn_action_type) : typingEnv =
    self#echoln 3 "\n    < TYPING SPAWN ACTION > started";
    let (Def def) = lookup_def m a#defName in
    let ts = List.map snd def#params in
      a#setArgTypes ts;
      env
	
  method spawnAction (env : typingEnv) (m : module_type) (d : definition_type) (p : process prefix_process_type) (a : spawn_action_type) (errs : typeErrors list) : typeErrors =
    self#echoln 3 "\n    < TYPING SPAWN ACTION > finished";
    let (Def def) = lookup_def m a#defName in
    let ts = List.map snd def#params in
    let errl =
      List.fold_left2
	(fun l t1 t2 ->
	   if(type_eq t1 t2)then
	     l
	   else
	     TypeError ("Mismatch SPAWNVALUE § § § : ", (a :> ast_type))::l)
	[] a#argTypes ts
    in
      errl@(List.concat errs)
      
  (* prim action *)
  method primAction_val (env : typingEnv) (m : module_type) (d : definition_type) (p : process prefix_process_type) (a : prim_action_type) : typingEnv =
    self#echoln 3 "\n    < TYPING PRIM ACTION > started";
    let rec funct var_list type_list =
      match var_list with
	| [] -> []
	| head::tail ->
	    (match head with
	       | VVar (head) ->
		   (match lookup_env env head#name with
		      | None -> failwith ("Error: Unbound value " ^ head#name)
		      | Some (ty, _) -> ty::(funct tail (List.tl type_list)))
	       | VPrim (head) ->
		   let prim = PrimitiveUtils.get_value_type head#moduleName head#primName in 
		     prim#return::(funct tail (List.tl type_list))
	       | _ -> (List.hd type_list)::(funct tail (List.tl type_list)))
    in
      a#setArgTypes (List.rev (funct (List.rev a#args) (List.rev a#argTypes)));
      env
	
  method primAction (env : typingEnv) (m : module_type) (d : definition_type) (p : process prefix_process_type) (a : prim_action_type) (errs : typeErrors list) : typeErrors =
    self#echoln 3 "\n    < TYPING PRIM ACTION > finished";
    let prim = PrimitiveUtils.get_value_type a#moduleName a#primName in
      (if(not(prim#arity = a#arity))then
	 failwith ("Arity Error: This primitive has arity of size " ^  (string_of_int a#arity) ^ " but an the primitive should have arity of size" ^ (string_of_int prim#arity))
       else
	 ());
	let errl =
	  List.fold_left2
	    (fun l t1 t2 ->
	       if(type_eq t1 t2)then
		 l
	       else
		 TypeError ("Mismatch PRIM § § § : ", (a :> ast_type))::l)
	    [] prim#params a#argTypes
	in
	  errl@(List.concat errs)
      
  (* let action *)
  method letAction_val (env : typingEnv) (m : module_type) (d : definition_type) (p : process prefix_process_type) (a : let_action_type) : typingEnv =
    self#echoln 2 "\n    < TYPING LET ACTION > started";
    env
      
  method letAction (env : typingEnv) (m : module_type) (d : definition_type) (p : process prefix_process_type) (a : let_action_type) (errs: typeErrors) : typeErrors =
    self#echoln 2 "\n    < TYPING LET ACTION > finished";
    (match a#value with
       | VVar (v) ->
	   (match lookup_env env v#name with
	      | Some (ty, _) -> a#setValueType ty;
	      | None -> ()) (*[TypeError (("Unbound variable " ^ v#name), (v :> ast_type))]@errs)*)
       | VPrim (p) -> a#setValueType p#returnType;
       | VTuple (t) -> () 
       | _ -> ());
    if(type_eq a#variableType a#valueType)then
      errs
    else
      [TypeError (("Type Error: This expression has type " ^  (string_of_valueType a#variableType) ^ " but an expression was expected of type " ^ (string_of_valueType a#valueType), (a :> ast_type)))]
      
  (* process *)
  (* branches *)
  method branch_val (env : typingEnv) (m : module_type) (d : definition_type) (c : process choice_process_type) (i : int) (p : process prefix_process_type) : typingEnv =
    self#echoln 5 "\n    < TYPING BRANCH > started";
    match p#action with
      | Input (a) ->
	  (match lookup_env env a#channel with
	     | Some (TChan (var_ty), binder) -> SMap.add a#variable (var_ty, (a :> ast_binder_type)) env
	     | Some (_, binder)-> SMap.add a#variable (TBool, (a :> ast_binder_type)) env (* Erreur !!*)
	     | None -> env)	  
      | New (a) -> SMap.add a#variable (a#variableType, (a :> ast_binder_type)) env
      | Let (a) -> SMap.add a#variable (a#variableType, (a :> ast_binder_type)) env
      | _ -> env
	  
  method branch (env : typingEnv) (m : module_type) (d : definition_type) (parent : process choice_process_type) (index : int) (p : process prefix_process_type) (guardErrs : typeErrors) (actErrs : typeErrors) (continuationErrs : typeErrors) : typeErrors =
    self#echoln 5 "\n    < TYPING BRANCH > finished";
    let terrs =
      match p#guardType with
	| TBool -> guardErrs
	| _ -> [TypeError ("Mismatch type in guard ", (p :> ast_type))]@guardErrs
    in
    terrs@actErrs@continuationErrs
      
  (* process term *)
  method term_val (env : typingEnv) (m : module_type) (d : definition_type) (p : term_process_type) : unit =
    self#echoln 5 "\n    < TYPING END > started";
    ()
      
  method term (env : typingEnv) (m : module_type) (d : definition_type) (p : term_process_type) : typeErrors =
    self#echoln 5 "\n    < TYPING END > finished";
    []
      
  (* process call *)
  method call_val (env : typingEnv) (m : module_type) (d : definition_type) (p : call_process_type) : typingEnv =
    self#echoln 4 "\n    < TYPING CALL > started";
    let (Def def) = lookup_def m p#defName in
    let ts = List.map snd def#params in
      p#setArgTypes ts;
      env

  method call (env : typingEnv) (m : module_type) (d : definition_type) (p : call_process_type) (errs : typeErrors list) : typeErrors =
    self#echoln 4 "\n    < TYPING CALL > finished";
    List.concat errs

  (* process choice *)
  method choice_val (env : typingEnv) (m : module_type) (d : definition_type) (p : process choice_process_type) : typingEnv =
    self#echoln 5 "\n    < TYPING CHOICE > started";
    env
      
  method choice (env : typingEnv) (m : module_type) (d : definition_type) (p : process choice_process_type) (errs : typeErrors list) : typeErrors =
    self#echoln 5 "\n    < TYPING CHOICE > finished";
    List.flatten errs
      
  (* definitions *)
  method definition_val (env : typingEnv) (m : module_type) (d : definition_type) : typingEnv = 
    self#echoln 2 ("\n    < TYPING DEFINITION > " ^ d#name ^ " started");
    List.fold_left 
      (fun env (name, typ) -> SMap.add name (typ, (d :> ast_binder_type)) env) SMap.empty d#params
      
  method definition (env : typingEnv) (m : module_type) (d : definition_type) (errs : typeErrors) : typeErrors = 
    self#echoln 2 "\n    < TYPING DEFINITION > finished";
    errs 
      
  (* module *)
  method moduleDef_val (m : module_type) : typingEnv = 
    self#echoln 2 "\n    < TYPING_MODULE > Typing pass started";
    SMap.empty
      
  method moduleDef (m : module_type) (errs : typeErrors list) : typeErrors =
    self#echoln 2 "\n    < TYPING_MODULE > Typing pass finished";
    let errs' = List.fold_left (fun es es' -> es@es') [] errs in
      if(empty_list errs')then  
	(self#echoln 1 (" \n  ===> Type Checking : success, no low-level type error \n"); 
	 errs')
      else
	(List.iter (fun err -> self#echoln 1 (string_of_typeError err)) errs';
	 errs')
	  
end
  
let typing_pass n = ((new typing_pass_node n) :> (typingEnv, typeErrors) ASTUtils.fold_node);;
