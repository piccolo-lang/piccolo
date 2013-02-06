(* module Middleend
  ---------------

  Middleend passes

*)
(** This module defines the middle end part of the compiler. *)

open Utils;;
open Syntax;;
open Typing;;

(** Representation of a [string list,int] fold_node. 
    Compute esize and attribute De Bruijn index *)

(* [TODO] attribuer les indices aux paramètres *)
class env_compute_pass (n:int) : [string list, int] ASTUtils.fold_node = 
  let lookup env v = 
    let rec aux env n = match env with
      | [] -> None
      | w::env' -> if v=w then Some n else aux env' (n+1)
    in aux env 0
  in
object(self)
  (* config *)
  method verbosity = n
  method echo vn str = if vn<=n then print_string str
  method echoln vn str = if vn<=n then print_endline str
      
  (* module *)
  method moduleDef_val m = []
  method moduleDef m esizes = 
    self#echoln 2 ("env pass finished in Module: " ^ m#name);
    list_max esizes
  (* definitions *)
  method definition_val _ m (d:definition_type) = 
    (* List.iter (fun (n,_) -> d#extendEnv n) d#params ; -- constructed in the parsing *)
    d#env
  method definition _ m d esize = 
    self#echoln 2 ("env pass finished in Definition: " ^ d#name) ;
    let esize' = d#arity + esize
    in 
    self#echoln 2 (" ==> computed env size = " ^ (string_of_int esize')) ;
    esize'
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
	env @ [a#variable]
      | Some n -> a#setVariableIndex n ; env)
    
    | Output a ->
      (match (lookup env a#channel) with
      | None -> () (* an error will be return by the typing pass *)
      | Some n -> a#setChannelIndex n);
      env
    
    | New a ->  
      (match (lookup env a#variable) with
      | None -> a#setVariableIndex (List.length env) ; 
	d#extendEnv a#variable;
	env @ [a#variable]
      | Some n -> a#setVariableIndex n ; env)
	
    | Let a ->  (match (lookup env a#variable) with
      | None -> a#setVariableIndex (List.length env) ; 
	d#extendEnv a#variable;
	env @ [a#variable]
      | Some n -> a#setVariableIndex n ; env)
    | _ -> env
  method branch env m d p i b s1 s2 s3 = s1+s2+s3
  method call_val env m d p = env
  method call env m d p _ = p#arity 
  (* Note: a second pass must refine this because the call arity may be not enough *)
  method term_val env m d p = ()
  method term env m d p = 0
  (* actions *)
  method outAction_val env m d p a = env
  method outAction env m d p a r = 0
  method inAction_val env m d p a = () 
  method inAction env m d p a =
    match (lookup env a#variable) with
    | None -> 1
    | Some _ -> 0
  method tauAction_val env m d p a = ()
  method tauAction env m d p a = 0
  method newAction_val env m d p a = ()
  method newAction env m d p a =
    match (lookup env a#variable) with
    | None -> 1
    | Some _ -> 0
  method spawnAction_val env m d p a = env
  method spawnAction env m d p a rs = 0
  method primAction_val env m d p a = env
  method primAction env m d p a rs = 0
  method letAction_val env m d p a = env
  method letAction env m d p a r =  
    match (lookup env a#variable) with
    | None -> 1
    | Some _ -> 0
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
    | None -> (* generate an error and save in module errors ??? *) ()
    | Some n -> v#setIndex n (* or in a further pass, just test if index is still -1 *)
  method varValue env m d p t v = 0
  method primValue_val env m d p t v = env
  method primValue env m d p t v rs = 0
end

(** Representation of a iter_fold_node *)
class csize_compute_pass (n:int) : ASTUtils.iter_fold_node = 
object(self) 
  inherit ASTUtils.abstract_iter_fold_node_repr n
  method moduleDef_post m = self#echoln 2 ("csize pass in Module: " ^ m#name)
end

(**  *)
let first_pass m v = 
  ASTUtils.fold_module m 
    (ASTUtils.fold_seq (new csize_compute_pass v) 
       (ASTUtils.fold_compose 
          (new env_compute_pass v)
          (typing_pass v)))


