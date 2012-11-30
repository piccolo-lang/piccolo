
(* module Typing

   Type checking and (simple) inference

*)

open Utils ;;
open Types ;;
open Syntax ;;

type typeError =
| TypeWarning of (string * ast_type)
| TypeError of (string * ast_type)

let string_of_typeError = function
  | TypeWarning (msg,ast) -> "Warning at " ^ (ast#posString) ^ "\n  ==> " ^ msg
  | TypeError(msg, ast) -> "Error at "  ^ (ast#posString) ^ "\n  ==> " ^ msg

type typeErrors = typeError list

let string_of_typeErrors errs = string_of_collection "" "" "\n" string_of_typeError errs

class typing_pass_node (verbosity:int) = 
object(self)
  inherit [typeErrors] ASTUtils.simple_abstract_fold_node_repr verbosity

  (* module *)
  method moduleDef_val (m:module_type) : unit =
    self#echoln 2 "Low-level Typing pass started"
  method moduleDef_fold (m:module_type) (errs:typeErrors list) : typeErrors =
    self#echoln 2 "Low-level Typing pass finished" ;
    let errs' = List.fold_left (fun es es' -> es@es') [] errs
    in
    if empty_list errs'
    then begin 
      self#echoln 2 ("  ==> no low-level type error") ; errs'
    end else begin
      List.iter (fun err -> self#echoln 1 (string_of_typeError err)) errs' ;
      errs'
    end

  (* definition *)
  method definition_fold: module_type -> definition_type -> typeErrors -> typeErrors =
    failwith "definition_fold: not yet implemented"

  (* process *)
  method choice_fold: module_type -> definition_type -> process choice_process_type -> typeErrors list -> typeErrors =
    failwith "choice_fold: not yet implemented"
  method branch_fold: module_type -> definition_type -> process choice_process_type -> int -> process prefix_process_type -> typeErrors -> typeErrors -> typeErrors -> typeErrors =
    failwith " branch_fold: not yet implemented"
  method call_fold: module_type -> definition_type ->  call_process_type -> typeErrors list -> typeErrors =
    failwith "call_fold: not yet implemented"
  method term_fold (m:module_type) (d:definition_type) (p:term_process_type) : typeErrors =
    self#echoln 3 "-- Typing term" ;
    []

  (* action *)
  method outAction_fold (m:module_type) (d:definition_type) (p:process prefix_process_type) (a:out_action_type) (errs:typeErrors) : typeErrors =
    self#echoln 3 "-- Typing output" ;
    (if a#channelIndex == -1
     then [TypeError("Channel variable '" ^ a#channel ^ "' not in scope",(a:>ast_type))]
     else []) @
      (match (a#channelType, a#valueType) with
      | (TChan t1,t2) -> 
        if type_eq t1 t2 then []
        else [TypeError("Mismatch channel type, expecting '" ^ (string_of_valueType (TChan t2)), (a:>ast_type))]
      | (_,t2) -> [TypeError("Not a channel type, expecting '" ^ (string_of_valueType (TChan t2)), (a:>ast_type))]) @
      errs
  method inAction_fold: module_type -> definition_type -> process prefix_process_type ->  in_action_type -> typeErrors =
    failwith "inAction_fold: not yet implemented"
  method tauAction_fold (m:module_type) (d:definition_type) (p:process prefix_process_type) (a:tau_action_type) : typeErrors =
    self#echoln 3 "-- Typing tau" ;
    []
  method newAction_fold: module_type -> definition_type -> process prefix_process_type ->  new_action_type -> typeErrors =
    failwith "newAction_fold: not yet implemented"
  method spawnAction_fold: module_type -> definition_type -> process prefix_process_type ->  spawn_action_type -> typeErrors list -> typeErrors =
    failwith "spawnAction_fold: not yet implemented"
  method primAction_fold: module_type -> definition_type -> process prefix_process_type ->  prim_action_type -> typeErrors list -> typeErrors =
    failwith "primAction_fold: not yet implemented"
  method letAction_fold: module_type -> definition_type -> process prefix_process_type ->  let_action_type -> typeErrors-> typeErrors =
    failwith "letAction_fold: not yet implemented"
  
  (* value *)
  method trueValue_fold (m:module_type) (d:definition_type) (p:process_type) (t:Types.valueType) (v:bool  const_value_type) : typeErrors =
    self#echoln 3 "-- Typing constant true" ;
    let errs = match t with
      | TBool -> []
      | _ -> [TypeError("Mismatch type '" ^ (string_of_valueType t) ^ "' for constant, expected Bool", (v:>ast_type))]
    in                                                                                                         v#setType TBool;
    errs
  method falseValue_fold: module_type -> definition_type -> process_type -> Types.valueType -> bool  const_value_type -> typeErrors =
    failwith "falseValue_fold: not yet implemented"
  method intValue_fold: module_type -> definition_type -> process_type -> Types.valueType -> int  const_value_type -> typeErrors =
    failwith "intValue_fold: not yet implemented"
  method stringValue_fold (m:module_type) (d:definition_type) (p:process_type) (t:Types.valueType) (v:string  const_value_type) : typeErrors =
    self#echoln 3 "-- Typing string constant" ;
    let errs = match t with
      | TString -> []
      | _ -> [TypeError("Mismatch type '" ^ (string_of_valueType t) ^ "' for constant, expected String", (v:>ast_type))]
    in                                                                                                         v#setType TString;
    errs
  method tupleValue_fold: module_type -> definition_type -> process_type -> Types.valueType -> value  tuple_value_type -> typeErrors list -> typeErrors =
    failwith "tupleValue_fold: not yet implemented"
  method varValue_fold: module_type -> definition_type -> process_type -> Types.valueType ->  variable_type -> typeErrors =
    failwith " varValue_fold: not yet implemented"
  method primValue_fold: module_type -> definition_type -> process_type -> Types.valueType ->  value prim_value_type -> typeErrors list -> typeErrors =
    failwith "primValue_fold: not yet implemented"
end

let typing_pass n = ((new typing_pass_node n) :> (unit,typeErrors) ASTUtils.fold_node)

(*
  let checkAndInferTypes_out m d out =
  (let vref = d#envRef out#channel
  if vref == -1 then [ TypeError ("Unknown channel variable '" ^ out#channel ^ "'", (out :> ast_type)) ]
  else out#setChannelRef vref ; []) 
  @ (checkAndInferTypes_value m d out#valueType out#value 

  let checkAndInferTypes_action m d act = match act with
  | Tau -> []
  | Output o -> checkAndInferTypes_out m d o
  | Input i -> checkAndInferTypes_in m d i
  | New n -> checkAndInferTypes_new m d n
  | Spawn s -> checkAndInferTypes_spawn m d s
  | Prim p -> checkAndInferTypes_prim m d p
  | Let l -> checkAndInferTypes_let m d l

  let checkAndInferTypes_branch m d c branch =
  let errs = 
  (let result = checkAndInferTypes_value m d branch#guardType branch#guard
  in match result with
  | Left errs -> errs
  | Right t -> branch#setGuardType t ; [])
  @ (checkAndInferTypes_action m d branch#action)
  @ (checkAndInferTypes_proc m d branch#proc)
  in errs

  let checkAndInferTypes_choice m d choice =
  let rec aux branches errs = function
  | [] -> errs
  | b::bs -> aux bs (errs @ checkAndInferTypes_branch m d choice b)
  in
  aux choice#branches []

  let checkAndInferTypes_callArg call ptype atype = match (ptype,atype) with
  | (TUnknown, TUnknown) -> Left (TypeError ("Parameter and argument both have undefined types", call))
  | (TUnknown, t) -> Right t
  | (t,TUnknown) -> Right t
  | (t1,t2) -> 
  if t1=t2 then Right t1
  else Left (TypeError ("Mismatch types in call expected " ^ (string_of_valueType t1) ^ " found " ^ (string_of_valueType t2),call))

  let checkAndInferTypes_callArgs call ptypes atypes = 
  let rec aux call ptypes atypes ts errs =
  match (ptypes,atypes) with
  | ([], []) -> if empty_list errs then Right (List.rev ts) else Left (List.rev errs) 
  | (ptype::ptypes',atype::atypes') -> (match checkAndInferTypes_callArg call ptype atype with
  | Left err -> aux call ptypes' atypes' (atype::ts) (err::errs)
  | Right t -> aux call ptypes' atypes' (t::ts) errs)
  | _ -> Left (List.rev ((TypeError ("Arity issue (please report)", call))::errs))
  in aux call ptypes atypes [] []

  let checkAndInferTypes_call m d call = 
  if m#name != call#moduleName then [ TypeError ("External calls not (yet) supported", (call :> ast_type)) ]
  else try 
  let callDef = m#lookupDef call#defName
  in match callDef with
  | Def def -> if call#arity != def#arity then [ TypeError ("Wrong number of arguments: expected " ^ (string_of_int def#arity) ^ " given " ^ (string_of_int call#arity), (call :> ast_type)) ]
  else let result : (typeErrors,valueType list) either = checkAndInferTypes_callArgs (call:>ast_type) (List.map second def#params) call#argTypes
  in match result with
  | Left errs -> errs
  | Right types -> call#setArgTypes types ; def#setParamTypes types ; [] (* no error *)
  with Not_found -> [ TypeError ("No such definition: " ^ call#defName, (call :> ast_type)) ]
  
  let checkAndInferTypes_proc m d proc = match proc with
  | Term _ -> []
  | Call c -> checkAndInferTypes_call m d c
  | Choice c -> checkAndInferTypes_choice m d c

  let checkAndInferTypes_def m def = match def with
  | Def d -> 
  checkAndInferTypes_proc m d d#process

  let checkAndInferTypes = function
  | Module m -> List.fold_left (fun errs def -> errs @ checkAndInferTypes_def m def) [] m#definitions

*)
