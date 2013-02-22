(* module Typing
   -------------
   
   Type checking and (simple) inference

*)
(** this module executes a type checking pass used in MiddleEnd *)

open Utils;;
open Types;;
open Syntax;;

(** type representing typeError *)
type typeError =
  | TypeWarning of (string * ast_type)
  | TypeError of (string * ast_type)
;;
      
(** string representing a typeError *)
let string_of_typeError = function
  | TypeWarning (msg, ast) -> "Warning at " ^ (ast#posString) ^ "\n  ==> " ^ msg
  | TypeError(msg, ast) -> "Error at "  ^ (ast#posString) ^ "\n  ==> " ^ msg
;;
      
(** type representing a list of typeErrors *)      
type typeErrors = typeError list;;
    
(** string representing a list of typeErrors *)
let string_of_typeErrors errs = string_of_collection "" "" "\n" string_of_typeError errs;;
  
(** representation of the type of a fold_node *)
type typingEnv = (valueType * ast_binder_type) SMap.t;;

(** string representing a typing_env *)
let print_typingEnv env =
  List.iter (fun ((name, ty), binder) -> Printf.printf "%s:%s (%s)\n" name (string_of_valueType ty) binder#toString ) env;
  Printf.printf "\n%!"
;;

(** Thread a typing env which is enriched with definitions and choice_process
  * return a list of errors wich is printed in level 2 verbosity
  *)
class typing_pass_node (n : int) : [typingEnv, typeErrors] ASTUtils.fold_node = 
  let lookup env v = 
    try 
      Some(SMap.find v env)
    with Not_found -> None
  in
object(self)
  (* level of verbosity for the output messages *)
  method verbosity = n
  
  method echo vn str = if vn <= n then print_string str
    
  method echoln vn str = if vn <= n then print_endline str

  (* value *)
  (* value true *)
  method trueValue_val (env : typingEnv) (m : module_type) (d : definition_type) (p : process_type) (t : Types.valueType) (v : bool const_value_type) : unit =
    self#echoln 4 "\n[TYPING BOOLEAN] started";
    ()
      
  method trueValue (env : typingEnv) (m : module_type) (d : definition_type) (p : process_type) (t : Types.valueType) (v : bool const_value_type) : typeErrors =
    self#echoln 4 "\n[TYPING CONSTANT TRUE] finished";
    let errs =
      match t with
	| TBool -> []
	| _ -> [TypeError("Mismatch type '" ^ (string_of_valueType t) ^ "' for constant, expected Bool", (v :> ast_type))]
    in
      v#setType TBool;
      errs
	
  (* value false *)
  method falseValue_val (env : typingEnv) (m : module_type) (d : definition_type) (p : process_type) (t : Types.valueType) (v : bool const_value_type) : unit =
    self#echoln 4 "\n[TYPING BOOLEAN] started";
    ()
    
  method falseValue (env : typingEnv) (m : module_type) (d : definition_type) (p : process_type) (t : Types.valueType) (v : bool const_value_type) : typeErrors =
    self#echoln 4 "\n[TYPING CONSTANT FALSE] finished";
    let errs =
      match t with
	| TBool -> []
	| _ -> [TypeError("Mismatch type '" ^ (string_of_valueType t) ^ "' for constant, expected Bool", (v :> ast_type))]
    in
      v#setType TBool;
      errs

  (* value int *)
  method intValue_val (env : typingEnv) (m : module_type) (d : definition_type) (p : process_type) (t : Types.valueType) (v : int const_value_type) : unit = 
    self#echoln 4 "\n[TYPING INTEGER] started";
    ()
  
  method intValue (env : typingEnv) (m : module_type) (d : definition_type) (p : process_type) (t : Types.valueType) (v : int const_value_type) : typeErrors =
    self#echoln 4 "\n[TYPING CONSTANT INT] finished";
    let errs =
      match t with
	| TInt -> []
	| _ -> [TypeError("Mismatch type '" ^ (string_of_valueType t) ^ "' for constant, expected Int", (v:>ast_type))]
    in
      v#setType TInt;
      errs

  (* value string *)
  method stringValue_val (env : typingEnv) (m : module_type) (d : definition_type) (p : process_type) (t : Types.valueType) (v : string const_value_type) : unit =
    self#echoln 4 "\n[TYPING STRING] started";
    ()
  
  method stringValue (env : typingEnv) (m : module_type) (d : definition_type) (p : process_type) (t : Types.valueType) (v : string const_value_type) : typeErrors =
    self#echoln 4 "\n[TYPING CONSTANT STRING] finished";
    let errs =
      match t with
	| TString -> []
	| _ -> [TypeError("Mismatch type '" ^ (string_of_valueType t) ^ "' for constant, expected String", (v :> ast_type))]
    in
      v#setType TString;
      errs
	
  (* value tuple *)
  method tupleValue_val (env : typingEnv) (m : module_type) (d : definition_type) (p : process_type) (t : Types.valueType) (v : value tuple_value_type) : typingEnv =
    self#echoln 4 "\n[TYPING TUPLE] started";
    env
    
  method tupleValue (env : typingEnv) (m : module_type) (d : definition_type) (p : process_type) (t : Types.valueType) (v : value tuple_value_type) (errs : typeErrors list) : typeErrors =
    self#echoln 4 "\n[TYPING TUPLE] finished";
    failwith "tupleValue_fold: not yet implemented"

  (* value variable /!\type check/!\ *)
  method varValue_val (env : typingEnv) (m : module_type) (d : definition_type) (p : process_type) (t : Types.valueType) (v : variable_type) : unit =
    self#echoln 2 "\n[TYPING VARIABLE] started";
    ()
      
  method varValue env (m : module_type) (d : definition_type) (p : process_type) (t : Types.valueType) (v : variable_type) : typeErrors =
    self#echoln 2 "\n[TYPING VARIABLE] finished";
    (* print_typingEnv env; *)
    (if v#ofType <> TUnknown then Printf.printf "%s already typed ?? [check !!]\n%!" v#name);
    match lookup env v#name with
      | Some (TUnknown, binder) -> 
	  v#setBinder binder; 
	  self#echoln 5 (Printf.sprintf "ERROR setted to %s \n" v#toString);      
	  [TypeError (("Unknown type for "^v#name), (v :> ast_type))]
      | Some (ty, binder) -> 
	  failwith "under construction"
	    (*
	      if(type_eq ty t) then
	      (v#setType ty; 
	      v#setBinder binder; 
	      self#echoln 5 (Printf.sprintf "%s setted to %s \n" v#name v#toString);
	      []) *)
      | None -> [TypeError (("Unbound value "^v#name), (v :> ast_type))]
	  
  (* value primitive *)
  method primValue_val (env : typingEnv) (m : module_type) (d : definition_type) (p : process_type) (t : Types.valueType) (v : value prim_value_type) : typingEnv =
    self#echoln 4 "\n[TYPING PRIMITIVE] started";
    env
      
  method primValue (env : typingEnv) (m : module_type) (d : definition_type) (p : process_type) (t: Types.valueType) (v : value prim_value_type) (errs: typeErrors list) : typeErrors =
    self#echoln 4 "\n[TYPING PRIMITIVE] finished";
    failwith "primValue_fold: not yet implemented"

  (* action *)
  (* tau action *)
  method tauAction_val (env : typingEnv) (m : module_type) (d : definition_type) (p : process prefix_process_type) (a : tau_action_type) : unit =
    self#echoln 4 "\n[TYPING TAU ACTION] started";
    ()
      
  method tauAction (env : typingEnv) (m : module_type) (d : definition_type) (p : process prefix_process_type) (a : tau_action_type) : typeErrors =
    self#echoln 4 "\n[TYPING TAU ACTION] finished";
    []

  (* out actions /!\type check/!\ *)
  method outAction_val (env : typingEnv) (m : module_type) (d : definition_type) (p : process prefix_process_type) (a : out_action_type) : typingEnv =
    self#echoln 2 "\n[TYPING OUT ACTION] started";
    env
      
  method outAction (env : typingEnv) (m : module_type) (d : definition_type) (p : process prefix_process_type) (a : out_action_type) (errs : typeErrors) : typeErrors =
    self#echoln 2 "\n[TYPING OUT ACTION] finished";
    (match lookup env a#channel with
       | None -> [TypeError("Unbound channel " ^ a#channel, (a :> ast_type))]
       | Some (chType, _) ->
	   try 
	     let valType= 
	       match a#value with 
		 | VVar v -> 
		     (match lookup env v#name with
			| Some (ty, _) -> ty
			| None -> failwith v#name)
		 | _ -> a#valueType
	     in
	       begin
		 match (chType, valType) with
		   | (TChan t1, t2) ->
		       (* Printf.printf "##### Comparing %s with %s #####" (string_of_valueType t1) (string_of_valueType t2); *)
		       if type_eq t1 t2 then
			 []
		       else
			 [TypeError("Mismatch channel type, expecting '" ^ (string_of_valueType (TChan t2)), (a :> ast_type))]
		   | (_, t2) -> [TypeError("Not a channel type, expecting '" ^ (string_of_valueType (TChan t2)), (a :> ast_type))]
	       end
	   with Failure var_name -> [TypeError("Unbound variable " ^ var_name, (a :> ast_type))]
    )
    @ errs

  (* in action *)
  method inAction_val (env : typingEnv) (m : module_type) (d : definition_type) (p : process prefix_process_type) (a : in_action_type) : unit =
    self#echoln 2 "\n[TYPING INPUT ACTION] started";
    ()
  
  method inAction (env : typingEnv) (m : module_type) (d : definition_type) (p : process prefix_process_type) (a : in_action_type) : typeErrors =
    self#echoln 2 "\n[TYPING INPUT ACTION] finished";
    (if a#variableType <> TUnknown then Printf.printf "%s already typed ?? [check !!]\n%!" a#variable);
    match d#fetchBinderType a#channel with 
	(*binder can be something else than the definition correction !!! *)
	(* search binder and only if not found then error*)
      |	None -> [TypeError (("Unbound channel "^a#channel), (a :> ast_type)) ]
      | Some (TChan var_ty)-> 
	  a#setChannelBinder (d :> ast_binder_type);
	  a#setVariableType var_ty; 
	  self#echoln 3 ("---- setting var to type :" ^ (string_of_valueType var_ty));
	  []
      |Some _ -> [TypeError("Mismatch type for " ^ a#channel ^ " expecting Channel type'", (a :> ast_type))]
	 
  (* new action /!\ *)
  method newAction_val (env : typingEnv) (m : module_type) (d : definition_type) (p : process prefix_process_type) (a : new_action_type) : unit =
    self#echoln 2 "\n[TYPING NEW ACTION] started";
    ()
      
  method newAction (env : typingEnv) (m : module_type) (d : definition_type) (p : process prefix_process_type) (a : new_action_type) : typeErrors =
    self#echoln 2 "\n[TYPING NEW ACTION] finished";
    (*enrichir un environnement local ??*)
    []

  (* spawn action *)
  method spawnAction_val (env : typingEnv) (m : module_type) (d : definition_type) (p : process prefix_process_type) (a : spawn_action_type) : typingEnv =
    self#echoln 4 "\n[TYPING SPAWN ACTION] started";
    env
      
  method spawnAction (env : typingEnv) (m : module_type) (d : definition_type) (p : process prefix_process_type) (a : spawn_action_type) (errs : typeErrors list) : typeErrors =
    self#echoln 4 "\n[TYPING SPAWN ACTION] finished";
    List.flatten errs
      
  (* prim action *)
  method primAction_val (env : typingEnv) (m : module_type) (d : definition_type) (p : process prefix_process_type) (a : prim_action_type) : typingEnv =
    self#echoln 4 "\n[TYPING PRIM ACTION] started";
    env
      
  method primAction (env : typingEnv) (m : module_type) (d : definition_type) (p : process prefix_process_type) (a : prim_action_type) (errs : typeErrors list) : typeErrors =
    self#echoln 4 "\n[TYPING PRIM ACTION] finished";
    failwith "primAction: not yet implemented"
      
  (* let action /!\ type check /!\ *)
  method letAction_val (env : typingEnv) (m : module_type) (d : definition_type) (p : process prefix_process_type) (a : let_action_type) : typingEnv =
    self#echoln 2 "\n[TYPING LET ACTION] started";
    env
      
  method letAction (env : typingEnv) (m : module_type) (d : definition_type) (p : process prefix_process_type) (a : let_action_type) (errs: typeErrors) : typeErrors =
    self#echoln 2 "\n[TYPING LET ACTION] finished";
    failwith "letAction_fold: not yet implemented"
      
  (* process *)
  (* branches *)
  method branch_val (env : typingEnv) (m : module_type) (d : definition_type) (c : process choice_process_type) (i : int) (p : process prefix_process_type) : typingEnv = 
    self#echoln 2 "\n[TYPING BRANCH] started";
    match p#action with
      | Input a -> 
	  (match lookup env a#channel with
	     | Some (TChan var_ty, binder)-> SMap.add a#variable (var_ty, (a :> ast_binder_type)) env
	     | Some (_, binder)-> SMap.add a#variable (TUnknown, (a :> ast_binder_type)) env (* Erreur !!*)
	     | None -> env)	  
      | New(a) -> SMap.add a#variable (a#variableType, (a :> ast_binder_type)) env
      | Let(a) -> SMap.add a#variable (a#variableType, (a :> ast_binder_type)) env
      | _ -> env

  method branch (env : typingEnv) (m : module_type) (d : definition_type) (parent : process choice_process_type) (index : int) (p : process prefix_process_type) (guardErrs : typeErrors) (actErrs : typeErrors) (continuationErrs : typeErrors) : typeErrors =
    self#echoln 2 "\n[TYPING BRANCH] finished";
    guardErrs @ actErrs @ continuationErrs
      
  (* process term *)
  method term_val (env : typingEnv) (m : module_type) (d : definition_type) (p : term_process_type) : unit =
    self#echoln 4 "\n[TYPING END] started";
    ()

  method term (env : typingEnv) (m : module_type) (d : definition_type) (p : term_process_type) : typeErrors =
    self#echoln 2 "\n[TYPING END] finished";
    []
      
  (* process call *)
  method call_val (env : typingEnv) (m : module_type) (d : definition_type) (p : call_process_type) : typingEnv =
    self#echoln 3 "\n[TYPING CALL] started";
    env
      
  method call (env : typingEnv) (m : module_type) (d : definition_type) (p : call_process_type) (errs : typeErrors list) : typeErrors =
    self#echoln 3 "\n[TYPING CALL] finished";
    List.flatten errs
  
  (* process choice *)
  method choice_val (env : typingEnv) (m : module_type) (d : definition_type) (p : process choice_process_type) : typingEnv =
    self#echoln 4 "\n[TYPING CHOICE] started";
    env
      
  method choice (env : typingEnv) (m : module_type) (d : definition_type) (p : process choice_process_type) (errs : typeErrors list) : typeErrors =
    self#echoln 4 "\n[TYPING CHOICE] finished";
    List.flatten errs
	  
  (* definitions *)
  method definition_val (env : typingEnv) (m : module_type) (d : definition_type) : typingEnv = 
    self#echoln 2 ("\n[TYPING DEFINITION] " ^ d#name ^ " started");
    List.fold_left 
      (fun env (name, typ) -> 
	 SMap.add name (typ, (d :> ast_binder_type)) env) SMap.empty d#params
      
  method definition (env : typingEnv) (m : module_type) (d : definition_type) (errs : typeErrors) : typeErrors = 
    self#echoln 2 "\n[TYPING DEFINITION] finished";
    errs 
    
  (* module *)
  method moduleDef_val (m : module_type) : typingEnv = 
    self#echoln 2 "\n[TYPING_MODULE] Low-level Typing pass started";
    SMap.empty
      
  method moduleDef (m : module_type) (errs : typeErrors list) : typeErrors =
    self#echoln 2 "\n[TYPING_MODULE] Low-level Typing pass finished";
    let errs' = List.fold_left (fun es es' -> es@es') [] errs in
      if empty_list errs' then  
	(self#echoln 2 ("  ==> no low-level type error"); 
	 errs')
      else
	(List.iter (fun err -> self#echoln 1 (string_of_typeError err)) errs';
	 errs')
	  
end

let typing_pass n = ((new typing_pass_node n) :> (typingEnv, typeErrors) ASTUtils.fold_node);;
