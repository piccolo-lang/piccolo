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
  | TypeWarning (msg,ast) -> "Warning at " ^ (ast#posString) ^ "\n  ==> " ^ msg
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
class typing_pass_node (n:int) : [typingEnv, typeErrors] ASTUtils.fold_node = 
  let lookup env v = 
    try 
      Some(SMap.find v env)
    with Not_found -> None
  in
object(self)
  (* level of verbosity for the output messages *)
  method verbosity = n
    
  (* level *)
  method echo vn str = if vn <= n then print_string str

  (* level *)
  method echoln vn str = if vn <= n then print_endline str
    
  (* module *)
  method moduleDef_val (m:module_type) : typingEnv = 
    self#echoln 2 "Low-level Typing pass started";
    SMap.empty
  
  method moduleDef (m:module_type) (errs:typeErrors list) : typeErrors =
    self#echoln 2 "Low-level Typing pass finished" ;
    let errs' = List.fold_left (fun es es' -> es@es') [] errs in
      if empty_list errs' then 
	begin 
	  self#echoln 2 ("  ==> no low-level type error"); 
	  errs'
	end
      else
	begin
	  List.iter (fun err -> self#echoln 1 (string_of_typeError err)) errs';
	  errs'
	end

  (* definitions *)
  method definition_val _ (m:module_type) (d:definition_type) : typingEnv = 
    List.fold_left 
      (fun env (name, typ) -> 
	 SMap.add name (typ, (d:>ast_binder_type)) env) SMap.empty d#params
      
  method definition _ (m:module_type) (d:definition_type) (errs:typeErrors) : typeErrors = 
    self#echoln 2 ("-- typing pass finished in definition "^ d#name ^" [TODO] check if correct\n");
    errs 
      
  (* processes *)
  method choice_val env m d p = env
    
  method choice env (m:module_type) (d:definition_type) (p:process choice_process_type) (errs:typeErrors list) :typeErrors =
    self#echoln 3 "-- Typing choice [TODO] check if correct";
    List.flatten errs

  (* branches *)
  method branch_val (env:typingEnv) (m:module_type) (d:definition_type) (c:process choice_process_type) (i:int) (p:process prefix_process_type) : typingEnv = 
    match p#action with
      | Input a -> 
	  (match lookup env a#channel with
	     | Some (TChan var_ty, binder)-> SMap.add a#variable (var_ty, (a:>ast_binder_type)) env
	     | Some (_, binder)-> SMap.add a#variable (TUnknown, (a:>ast_binder_type)) env (* Erreur !!*)
	     | None -> env)	  
      | New a -> SMap.add a#variable (a#variableType, (a:>ast_binder_type)) env
      | Let a -> SMap.add a#variable (a#variableType, (a:>ast_binder_type)) env
      | _ -> env

  method branch env (m:module_type) (d:definition_type) (parent:process choice_process_type) (index:int) (p:process prefix_process_type) (guardErrs:typeErrors) (actErrs:typeErrors) (continuationErrs:typeErrors) :typeErrors =
    self#echoln 3 "-- Typing branch [TODO] check if correct";
    guardErrs @ actErrs @ continuationErrs
      
  (* call *)
  method call_val env m d p = env
    
  method call env (m:module_type) (d:definition_type) (p:call_process_type) (errs:typeErrors list) : typeErrors =
    self#echoln 3 "-- Typing call [TODO] !!!";
    List.flatten errs
      
  (* term *)
  method term_val env m d p = ()
  method term _ (m:module_type) (d:definition_type) (p:term_process_type) : typeErrors =
    self#echoln 5 "-- Typing term";
    []
      
  (* out actions *)
  method outAction_val env m d p a = env
    
  method outAction env (m:module_type) (d:definition_type) (p:process prefix_process_type) (a:out_action_type) (errs:typeErrors) : typeErrors =
    self#echoln 3 "-- Typing output";
    (match lookup env a#channel with
       | None -> [TypeError("Unbound channel " ^ a#channel, (a:>ast_type))]
       | Some (chType,_) ->
	   try 
	     let valType= 
	       match a#value with 
		 | VVar v -> 
		     (match lookup env v#name with
			| Some (ty,_) -> ty
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
			 [TypeError("Mismatch channel type, expecting '" ^ (string_of_valueType (TChan t2)), (a:>ast_type))]
		   | (_, t2) -> [TypeError("Not a channel type, expecting '" ^ (string_of_valueType (TChan t2)), (a:>ast_type))]
	       end
	   with Failure var_name -> [TypeError("Unbound variable " ^ var_name, (a:>ast_type))]
    )
    @ errs

  (* in action *)
  method inAction_val env m d p a = () 
  
  method inAction env (m:module_type) (d:definition_type) (p:process prefix_process_type) (a: in_action_type) :typeErrors =
    self#echoln 3 "-- Typing input";
    (if a#variableType <> TUnknown then Printf.printf "%s already typed ?? [check !!]\n%!" a#variable);
    match d#fetchBinderType a#channel with 
	(*binder can be something else than the definition correction !!! *)
	(* search binder and only if not found then error*)
      |	None -> [TypeError (("Unbound channel "^a#channel), (a:>ast_type)) ]
      | Some (TChan var_ty)-> 
	  a#setChannelBinder (d:>ast_binder_type);
	  a#setVariableType var_ty; 
	  self#echoln 3 ("---- setting var to type :" ^ (string_of_valueType var_ty));
	  []
      |Some _ -> [TypeError("Mismatch type for " ^ a#channel ^ " expecting Channel type'", (a:>ast_type))]

  (* tau action *)
  method tauAction_val env m d p a = ()
  
  method tauAction env (m:module_type) (d:definition_type) (p:process prefix_process_type) (a:tau_action_type) : typeErrors =
    self#echoln 5 "-- Typing tau";
    []

  (* new action *)
  method newAction_val env m d p a = ()
  
  method newAction env (m:module_type) (d:definition_type) (p:process prefix_process_type) (a:new_action_type) : typeErrors =
    self#echoln 3 "-- Typing new_action" ;
    (*enrichir un environnement local ??*)
    []

  method spawnAction_val env m d p a = env
  method spawnAction env (m:module_type) (d:definition_type) (p:process prefix_process_type) (s:spawn_action_type) (errs:typeErrors list) :typeErrors =
    self#echoln 3 "-- Typing spawn" ;
    List.flatten errs

  method primAction_val env m d p a = env
  method primAction : typingEnv -> module_type -> definition_type -> process prefix_process_type ->  prim_action_type -> typeErrors list -> typeErrors =
    failwith "primAction: not yet implemented"

  method letAction_val env m d p a = env
  method letAction env (m:module_type) (d:definition_type) (p:process prefix_process_type) (a:let_action_type) (errs: typeErrors): typeErrors =
    failwith "letAction_fold: not yet implemented"

  (* value *)
  method trueValue_val env m d p t v = ()
  method trueValue env (m:module_type) (d:definition_type) (p:process_type) (t:Types.valueType) (v:bool const_value_type) : typeErrors =
    self#echoln 3 "-- Typing constant true" ;
    let errs = match t with
      | TBool -> []
      | _ -> [TypeError("Mismatch type '" ^ (string_of_valueType t) ^ "' for constant, expected Bool", (v:>ast_type))]
    in
      v#setType TBool;
      errs

  method falseValue_val env m d p t v = ()
  method falseValue env (m:module_type) (d:definition_type) (p:process_type) (t:Types.valueType) (v:bool const_value_type) : typeErrors =
    self#echoln 3 "-- Typing constant false" ;
    let errs = match t with
      | TBool -> []
      | _ -> [TypeError("Mismatch type '" ^ (string_of_valueType t) ^ "' for constant, expected Bool", (v:>ast_type))]
    in
      v#setType TBool;
      errs

  method intValue_val env m d p t v = ()
  method intValue env (m:module_type) (d:definition_type) (p:process_type) (t:Types.valueType) (v:int const_value_type) : typeErrors =
    self#echoln 3 "-- Typing constant int" ;
    let errs = match t with
      | TInt -> []
      | _ -> [TypeError("Mismatch type '" ^ (string_of_valueType t) ^ "' for constant, expected Int", (v:>ast_type))]
    in
      v#setType TInt;
      errs

  method stringValue_val env m d p t v = ()
  method stringValue env (m:module_type) (d:definition_type) (p:process_type) (t:Types.valueType) (v:string const_value_type) : typeErrors =
    self#echoln 3 "-- Typing string constant" ;
    let errs = match t with
      | TString -> []
      | _ -> [TypeError("Mismatch type '" ^ (string_of_valueType t) ^ "' for constant, expected String", (v:>ast_type))]
    in
      v#setType TString;
      errs

  method tupleValue_val env m d p t v = env
  method tupleValue env (m: module_type) (d: definition_type) (p: process_type) (t: Types.valueType) (v: value tuple_value_type) (errs: typeErrors list) : typeErrors =
    failwith "tupleValue_fold: not yet implemented"

  method varValue_val env m d p t v = ()
  method varValue env (m:module_type) (d:definition_type) (p:process_type) (t:Types.valueType) (v: variable_type) : typeErrors =
    self#echoln 3 "-- Typing variable";
    (* print_typingEnv env; *)
    (if v#ofType <> TUnknown then Printf.printf "%s already typed ?? [check !!]\n%!" v#name);
    match lookup env v#name with
      | Some (TUnknown, binder) -> 
	  v#setBinder binder; 
	  self#echoln 5 (Printf.sprintf "ERROR setted to %s \n" v#toString);      
	  [TypeError (("Unknown type for "^v#name), (v:>ast_type))]
      | Some (ty, binder) -> 
	  v#setType ty; 
	  v#setBinder binder; 
	  self#echoln 5 (Printf.sprintf "%s setted to %s \n" v#name v#toString);
	  []
      | None -> [ TypeError (("Unbound value "^v#name), (v:>ast_type)) ]
  method primValue_val env m d p t v = env
  method primValue: typingEnv -> module_type -> definition_type -> process_type -> Types.valueType ->  value prim_value_type -> typeErrors list -> typeErrors =
    failwith "primValue_fold: not yet implemented"
end

let typing_pass n = ((new typing_pass_node n) :> (typingEnv,typeErrors) ASTUtils.fold_node)


(* let checkAndInferTypes_out m d out = *)
(*   (let vref = d#envRef out#channel in *)
(*    if vref = -1 then  *)
(*      [ TypeError ("Unknown channel variable '" ^ out#channel ^ "'", (out :> ast_type)) ] *)
(*    else out#setChannelRef vref ; []) *)
(*   @ (checkAndInferTypes_value m d out#valueType out#value) *)

(* let checkAndInferTypes_action m d act = match act with *)
(*   | Tau -> [] *)
(*   | Output o -> checkAndInferTypes_out m d o *)
(*   | Input i -> checkAndInferTypes_in m d i *)
(*   | New n -> checkAndInferTypes_new m d n *)
(*   | Spawn s -> checkAndInferTypes_spawn m d s *)
(*   | Prim p -> checkAndInferTypes_prim m d p *)
(*   | Let l -> checkAndInferTypes_let m d l *)

(* let checkAndInferTypes_branch m d c branch = *)
(*   let errs =  *)
(*     (let result = checkAndInferTypes_value m d branch#guardType branch#guard *)
(*      in match result with *)
(*      | Left errs -> errs *)
(*      | Right t -> branch#setGuardType t ; []) *)
(*     @ (checkAndInferTypes_action m d branch#action) *)
(*     @ (checkAndInferTypes_proc m d branch#proc) *)
(*   in errs *)

(* let checkAndInferTypes_choice m d choice = *)
(*   let rec aux branches errs = function *)
(*     | [] -> errs *)
(*     | b::bs -> aux bs (errs @ checkAndInferTypes_branch m d choice b) *)
(*   in *)
(*   aux choice#branches [] *)

(* let checkAndInferTypes_callArg call ptype atype = match (ptype,atype) with *)
(*   | (TUnknown, TUnknown) -> Left (TypeError ("Parameter and argument both have undefined types", call)) *)
(*   | (TUnknown, t) -> Right t *)
(*   | (t,TUnknown) -> Right t *)
(*   | (t1,t2) ->  *)
(*     if t1=t2 then Right t1 *)
(*     else Left (TypeError ("Mismatch types in call expected " ^ (string_of_valueType t1) ^ " found " ^ (string_of_valueType t2),call)) *)

(* let checkAndInferTypes_callArgs call ptypes atypes =  *)
(*   let rec aux call ptypes atypes ts errs = *)
(*     match (ptypes,atypes) with *)
(*     | ([], []) -> if empty_list errs then Right (List.rev ts) else Left (List.rev errs)  *)
(*     | (ptype::ptypes',atype::atypes') -> (match checkAndInferTypes_callArg call ptype atype with *)
(*       | Left err -> aux call ptypes' atypes' (atype::ts) (err::errs) *)
(*       | Right t -> aux call ptypes' atypes' (t::ts) errs) *)
(*     | _ -> Left (List.rev ((TypeError ("Arity issue (please report)", call))::errs)) *)
(*   in aux call ptypes atypes [] [] *)

(* let checkAndInferTypes_call m d call =  *)
(*   if m#name != call#moduleName then [ TypeError ("External calls not (yet) supported", (call :> ast_type)) ] *)
(*   else try  *)
(* 	 let callDef = m#lookupDef call#defName *)
(* 	 in match callDef with *)
(* 	 | Def def -> if call#arity != def#arity then [ TypeError ("Wrong number of arguments: expected " ^ (string_of_int def#arity) ^ " given " ^ (string_of_int call#arity), (call :> ast_type)) ] *)
(* 	   else let result : (typeErrors,valueType list) either = checkAndInferTypes_callArgs (call:>ast_type) (List.map second def#params) call#argTypes *)
(* 		in match result with *)
(* 		| Left errs -> errs *)
(* 		| Right types -> call#setArgTypes types ; def#setParamTypes types ; [] (\* no error *\) *)
(*     with Not_found -> [ TypeError ("No such definition: " ^ call#defName, (call :> ast_type)) ] *)
  
(* let checkAndInferTypes_proc m d proc = match proc with *)
(*   | Term _ -> [] *)
(*   | Call c -> checkAndInferTypes_call m d c *)
(*   | Choice c -> checkAndInferTypes_choice m d c *)

(* let checkAndInferTypes_def m def = match def with *)
(*   | Def d ->  *)
(*     checkAndInferTypes_proc m d d#process *)

(* let checkAndInferTypes = function *)
(*   | Module m -> List.fold_left (fun errs def -> errs @ checkAndInferTypes_def m def) [] m#definitions *)


