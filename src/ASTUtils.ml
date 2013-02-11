
(* module ASTUtils

AST utilities (traversal, etc.)

*)

open Utils ;;
open Types ;;
open Syntax ;;

(* --------------------- *)
(* AST Folding framework *)
(* --------------------- *)

(** Common interface to use with fold_module *)
class type ['a,'b] fold_node = object

  (* echo *)
  method verbosity: int
  method echo: int -> string -> unit
  method echoln: int -> string -> unit 
    
  (* module *)
  method moduleDef_val: module_type -> 'a
  method moduleDef: module_type -> 'b list -> 'b
  
  (* definition *)
  method definition_val: 'a -> module_type -> definition_type -> 'a
  method definition: 'a -> module_type -> definition_type -> 'b -> 'b
  
  (* process *)
  method choice_val: 'a -> module_type -> definition_type -> process choice_process_type -> 'a
  method choice: 'a -> module_type -> definition_type -> process choice_process_type -> 'b list -> 'b
  method branch_val: 'a -> module_type -> definition_type -> process choice_process_type -> int -> process prefix_process_type -> 'a
  method branch: 'a -> module_type -> definition_type -> process choice_process_type -> int -> process prefix_process_type -> 'b -> 'b -> 'b -> 'b
  method call_val: 'a -> module_type -> definition_type ->  call_process_type -> 'a
  method call: 'a -> module_type -> definition_type ->  call_process_type -> 'b list -> 'b
  method term_val: 'a -> module_type -> definition_type ->  term_process_type -> unit
  method term: 'a -> module_type -> definition_type ->  term_process_type -> 'b
  
  (* action *)
  method outAction_val: 'a -> module_type -> definition_type -> process prefix_process_type ->  out_action_type -> 'a
  method outAction: 'a -> module_type -> definition_type -> process prefix_process_type ->  out_action_type -> 'b -> 'b
  method inAction_val: 'a -> module_type -> definition_type -> process prefix_process_type ->  in_action_type -> unit
  method inAction: 'a -> module_type -> definition_type -> process prefix_process_type ->  in_action_type -> 'b
  method tauAction_val: 'a-> module_type -> definition_type -> process prefix_process_type ->  tau_action_type -> unit
  method tauAction: 'a -> module_type -> definition_type -> process prefix_process_type ->  tau_action_type -> 'b
  method newAction_val: 'a -> module_type -> definition_type -> process prefix_process_type ->  new_action_type -> unit
  method newAction: 'a -> module_type -> definition_type -> process prefix_process_type ->  new_action_type -> 'b
  method spawnAction_val: 'a -> module_type -> definition_type -> process prefix_process_type ->  spawn_action_type -> 'a
  method spawnAction: 'a -> module_type -> definition_type -> process prefix_process_type ->  spawn_action_type -> 'b list -> 'b
  method primAction_val: 'a -> module_type -> definition_type -> process prefix_process_type ->  prim_action_type -> 'a
  method primAction: 'a -> module_type -> definition_type -> process prefix_process_type ->  prim_action_type -> 'b list -> 'b
  method letAction_val: 'a -> module_type -> definition_type -> process prefix_process_type ->  let_action_type -> 'a
  method letAction: 'a -> module_type -> definition_type -> process prefix_process_type ->  let_action_type -> 'b-> 'b
  
  (* value *)
  method trueValue_val: 'a -> module_type -> definition_type -> process_type -> Types.valueType -> bool const_value_type -> unit
  method trueValue: 'a -> module_type -> definition_type -> process_type -> Types.valueType -> bool  const_value_type -> 'b
  
  method falseValue_val: 'a -> module_type -> definition_type -> process_type -> Types.valueType -> bool  const_value_type -> unit
  method falseValue: 'a -> module_type -> definition_type -> process_type -> Types.valueType -> bool  const_value_type -> 'b
  
  method intValue_val: 'a -> module_type -> definition_type -> process_type -> Types.valueType -> int  const_value_type -> unit
  method intValue: 'a -> module_type -> definition_type -> process_type -> Types.valueType -> int  const_value_type -> 'b
  
  method stringValue_val: 'a -> module_type -> definition_type -> process_type -> Types.valueType -> string  const_value_type -> unit
  method stringValue: 'a -> module_type -> definition_type -> process_type -> Types.valueType -> string  const_value_type -> 'b
  
  method tupleValue_val: 'a -> module_type -> definition_type -> process_type -> Types.valueType -> value  tuple_value_type -> 'a
  method tupleValue: 'a -> module_type -> definition_type -> process_type -> Types.valueType -> value  tuple_value_type -> 'b list -> 'b
  
  method varValue_val: 'a -> module_type -> definition_type -> process_type -> Types.valueType ->  variable_type -> unit
  method varValue: 'a -> module_type -> definition_type -> process_type -> Types.valueType ->  variable_type -> 'b
  
  method primValue_val: 'a -> module_type -> definition_type -> process_type -> Types.valueType ->  value prim_value_type -> 'a
  method primValue: 'a -> module_type -> definition_type -> process_type -> Types.valueType ->  value prim_value_type -> 'b list -> 'b
end

class type ['a] simple_fold_node = object 
  inherit [unit,'a] fold_node
  
  (* module *)
  method moduleDef_fold: module_type -> 'a list -> 'a
    
  (* definition *)
  method definition_fold: module_type -> definition_type -> 'a -> 'a
    
  (* process *)
  method choice_fold: module_type -> definition_type -> process choice_process_type -> 'a list -> 'a
  method branch_fold: module_type -> definition_type -> process choice_process_type -> int -> process prefix_process_type -> 'a -> 'a -> 'a -> 'a
  method call_fold: module_type -> definition_type ->  call_process_type -> 'a list -> 'a
  method term_fold: module_type -> definition_type ->  term_process_type -> 'a
    
  (* action *)
  method outAction_fold: module_type -> definition_type -> process prefix_process_type ->  out_action_type -> 'a -> 'a
  method inAction_fold: module_type -> definition_type -> process prefix_process_type ->  in_action_type -> 'a
  method tauAction_fold: module_type -> definition_type -> process prefix_process_type ->  tau_action_type -> 'a
  method newAction_fold: module_type -> definition_type -> process prefix_process_type ->  new_action_type -> 'a
  method spawnAction_fold: module_type -> definition_type -> process prefix_process_type ->  spawn_action_type -> 'a list -> 'a
  method primAction_fold: module_type -> definition_type -> process prefix_process_type ->  prim_action_type -> 'a list -> 'a
  method letAction_fold: module_type -> definition_type -> process prefix_process_type ->  let_action_type -> 'a-> 'a
    
  (* value *)
  method trueValue_fold: module_type -> definition_type -> process_type -> Types.valueType -> bool  const_value_type -> 'a
  method falseValue_fold: module_type -> definition_type -> process_type -> Types.valueType -> bool  const_value_type -> 'a
  method intValue_fold: module_type -> definition_type -> process_type -> Types.valueType -> int  const_value_type -> 'a
  method stringValue_fold: module_type -> definition_type -> process_type -> Types.valueType -> string  const_value_type -> 'a
  method tupleValue_fold: module_type -> definition_type -> process_type -> Types.valueType -> value  tuple_value_type -> 'a list -> 'a
  method varValue_fold: module_type -> definition_type -> process_type -> Types.valueType ->  variable_type -> 'a
  method primValue_fold: module_type -> definition_type -> process_type -> Types.valueType ->  value prim_value_type -> 'a list -> 'a
 end

class type virtual ['a] simple_abstract_fold_node = object 
  inherit [unit,'a] fold_node
  
  (* module *)
  method virtual moduleDef_fold: module_type -> 'a list -> 'a
  
  (* definition *)
  method virtual definition_fold: module_type -> definition_type -> 'a -> 'a
  
  (* process *)
  method virtual choice_fold: module_type -> definition_type -> process choice_process_type -> 'a list -> 'a
  method virtual branch_fold: module_type -> definition_type -> process choice_process_type -> int -> process prefix_process_type -> 'a -> 'a -> 'a -> 'a
  method virtual call_fold: module_type -> definition_type ->  call_process_type -> 'a list -> 'a
  method virtual term_fold: module_type -> definition_type ->  term_process_type -> 'a
  
  (* action *)
  method virtual outAction_fold: module_type -> definition_type -> process prefix_process_type ->  out_action_type -> 'a -> 'a
  method virtual inAction_fold: module_type -> definition_type -> process prefix_process_type ->  in_action_type -> 'a
  method virtual tauAction_fold: module_type -> definition_type -> process prefix_process_type ->  tau_action_type -> 'a
  method virtual newAction_fold: module_type -> definition_type -> process prefix_process_type ->  new_action_type -> 'a
  method virtual spawnAction_fold: module_type -> definition_type -> process prefix_process_type ->  spawn_action_type -> 'a list -> 'a
  method virtual primAction_fold: module_type -> definition_type -> process prefix_process_type ->  prim_action_type -> 'a list -> 'a
  method virtual letAction_fold: module_type -> definition_type -> process prefix_process_type ->  let_action_type -> 'a-> 'a
  
  (* value *)
  method virtual trueValue_fold: module_type -> definition_type -> process_type -> Types.valueType -> bool  const_value_type -> 'a
  method virtual falseValue_fold: module_type -> definition_type -> process_type -> Types.valueType -> bool  const_value_type -> 'a
  method virtual intValue_fold: module_type -> definition_type -> process_type -> Types.valueType -> int  const_value_type -> 'a
  method virtual stringValue_fold: module_type -> definition_type -> process_type -> Types.valueType -> string  const_value_type -> 'a
  method virtual tupleValue_fold: module_type -> definition_type -> process_type -> Types.valueType -> value  tuple_value_type -> 'a list -> 'a
  method virtual varValue_fold: module_type -> definition_type -> process_type -> Types.valueType ->  variable_type -> 'a
  method virtual primValue_fold: module_type -> definition_type -> process_type -> Types.valueType ->  value prim_value_type -> 'a list -> 'a
end

class virtual ['a] simple_abstract_fold_node_repr (n:int) : ['a] simple_abstract_fold_node = object(self)
  
  (* config *)
  method verbosity = n
  method echo vn str = if vn<=n then print_string str
  method echoln vn str = if vn<=n then print_endline str
  
  (* module *)
  method moduleDef_val m = ()
  method moduleDef m rs = self#moduleDef_fold m rs 
  method virtual moduleDef_fold: module_type -> 'a list -> 'a
  
  (* definition *)
  method definition_val v m d = ()
  method definition v m d r = self#definition_fold m d r
  method virtual definition_fold: module_type -> definition_type -> 'a -> 'a
  
  (* process *)
  method choice_val v m d p = ()
  method choice v m d p rs = self#choice_fold m d p rs
  method virtual choice_fold: module_type -> definition_type -> process choice_process_type -> 'a list -> 'a
  method branch_val v m d p (index:int) b = ()
  method branch (v:unit) m d p (index:int) b g a q = self#branch_fold m d p index b g a q
  method virtual branch_fold: module_type -> definition_type -> process choice_process_type -> int -> process prefix_process_type -> 'a -> 'a -> 'a -> 'a
  method call_val v m d p = ()
  method call v m d p = self#call_fold m d p
  method virtual call_fold: module_type -> definition_type ->  call_process_type -> 'a list -> 'a
  method term_val v m d p = ()
  method term v m d p = self#term_fold m d p
  method virtual term_fold: module_type -> definition_type ->  term_process_type -> 'a
  
  (* action *)
  method outAction_val v m d p a = ()
  method outAction v m d p a r = self#outAction_fold m d p a r
  method virtual outAction_fold: module_type -> definition_type -> process prefix_process_type ->  out_action_type -> 'a -> 'a
  method inAction_val v m d p a = ()
  method inAction v m d p a = self#inAction_fold m d p a
  method virtual inAction_fold: module_type -> definition_type -> process prefix_process_type ->  in_action_type -> 'a
  method tauAction_val v m d p a = ()
  method tauAction v m d p a = self#tauAction_fold m d p a
  method virtual tauAction_fold: module_type -> definition_type -> process prefix_process_type ->  tau_action_type -> 'a
  method newAction_val v m d p a = ()
  method newAction v m d p a = self#newAction_fold m d p a
  method virtual newAction_fold: module_type -> definition_type -> process prefix_process_type ->  new_action_type -> 'a
  method spawnAction_val v m d p a = ()
  method spawnAction v m d p a = self#spawnAction_fold m d p a
  method virtual spawnAction_fold: module_type -> definition_type -> process prefix_process_type ->  spawn_action_type -> 'a list -> 'a
  method primAction_val v m d p a = ()
  method primAction v m d p a rs = self#primAction_fold m d p a rs
  method virtual primAction_fold: module_type -> definition_type -> process prefix_process_type ->  prim_action_type -> 'a list -> 'a
  method letAction_val v m d p a = ()
  method letAction v m d p a r = self#letAction_fold m d p a r
  method virtual letAction_fold: module_type -> definition_type -> process prefix_process_type ->  let_action_type -> 'a-> 'a
  
  (* value *)
  method trueValue_val w m d p t v = ()
  method trueValue w m d p t v = self#trueValue_fold m d p t v
  method virtual trueValue_fold: module_type -> definition_type -> process_type -> Types.valueType -> bool  const_value_type -> 'a
  method falseValue_val w m d p t v = ()
  method falseValue w m d p t v = self#falseValue_fold m d p t v
  method virtual falseValue_fold: module_type -> definition_type -> process_type -> Types.valueType -> bool  const_value_type -> 'a
  method intValue_val w m d p t v = ()
  method intValue w m d p t v = self#intValue_fold m d p t v
  method virtual intValue_fold: module_type -> definition_type -> process_type -> Types.valueType -> int  const_value_type -> 'a
  method stringValue_val w m d p t v = ()
  method stringValue w m d p t v = self#stringValue_fold m d p t v
  method virtual stringValue_fold: module_type -> definition_type -> process_type -> Types.valueType -> string  const_value_type -> 'a
  method tupleValue_val w m d p t v = ()
  method tupleValue w m d p t v rs = self#tupleValue_fold m d p t v rs
  method virtual tupleValue_fold: module_type -> definition_type -> process_type -> Types.valueType -> value  tuple_value_type -> 'a list -> 'a
  method varValue_val w m d p t v = ()
  method varValue w m d p t v = self#varValue_fold m d p t v
  method virtual varValue_fold: module_type -> definition_type -> process_type -> Types.valueType ->  variable_type -> 'a
  method primValue_val w m d p t v = ()
  method primValue w m d p t v rs = self#primValue_fold m d p t v rs
  method virtual primValue_fold: module_type -> definition_type -> process_type -> Types.valueType ->  value prim_value_type -> 'a list -> 'a
end

class virtual  ['a,'b,'c,'d] merge_fold_node_repr (n1:('a,'b) fold_node) (n2:('c,'d) fold_node) = object
  
  (* module *)
  method moduleDef_val m = (n1#moduleDef_val m , n2#moduleDef_val m)
  
  (* definition *)
  method definition_val (v:('a*'c)) m d = (n1#definition_val (fst v) m d , n2#definition_val (snd v) m d)
  
  (* process *)
  method choice_val v m d p = (n1#choice_val (fst v) m d p , n2#choice_val (snd v) m d p)
  method branch_val v m d p index b = (n1#branch_val (fst v) m d p index b , n2#branch_val (snd v) m d p index b)
  method call_val v m d p = (n1#call_val (fst v) m d p , n2#call_val (snd v) m d p)
  method term_val v m d p = n1#term_val (fst v) m d p ; n2#term_val (snd v) m d p
  
  (* action *)
  method outAction_val v m d p a = (n1#outAction_val (fst v) m d p a , n2#outAction_val (snd v) m d p a)
  method inAction_val v m d p a = n1#inAction_val (fst v) m d p a ; n2#inAction_val (snd v) m d p a
  method tauAction_val v m d p a = n1#tauAction_val (fst v) m d p a ; n2#tauAction_val (snd v) m d p a
  method newAction_val (v:('a*'c)) m d p a = n1#newAction_val (fst v) m d p a ; n2#newAction_val (snd v) m d p a
  method spawnAction_val v m d p a = (n1#spawnAction_val (fst v) m d p a , n2#spawnAction_val (snd v) m d p a)
  method primAction_val  v m d p a = (n1#primAction_val (fst v) m d p a , n2#primAction_val (snd v) m d p a)
  method letAction_val v m d p a = (n1#letAction_val (fst v) m d p a , n2#letAction_val (snd v) m d p a)
  
  (* value *)  
  method trueValue_val w m d p t v = n1#trueValue_val (fst w) m d p t v ; n2#trueValue_val (snd w) m d p t v
  method falseValue_val w m d p t v = n1#falseValue_val (fst w) m d p t v ; n2#falseValue_val (snd w) m d p t v
  method intValue_val w m d p t v = n1#intValue_val (fst w) m d p t v ; n2#intValue_val (snd w) m d p t v
  method stringValue_val w m d p t v = n1#stringValue_val (fst w) m d p t v ; n2#stringValue_val (snd w) m d p t v
  method tupleValue_val w m d p t v = (n1#tupleValue_val (fst w) m d p t v , n2#tupleValue_val (snd w) m d p t v)
  method varValue_val w m d p t v = n1#varValue_val  (fst w) m d p t v ; n2#varValue_val (snd w) m d p t v
  method primValue_val w m d p t v = (n1#primValue_val (fst w) m d p t v , n2#primValue_val (snd w) m d p t v)
end

(**
   args:
   n1 n2
   method m
   m -> (n1#m , n2#m )
*)
class ['a,'b,'c,'d] compose_fold_node_repr (n1:('a,'b) fold_node) (n2:('c,'d) fold_node) : [('a*'c),('b * 'd)] fold_node = 
object(self)
  inherit ['a,'b,'c,'d] merge_fold_node_repr n1 n2
  
  (* config *)
  method verbosity = max (n1#verbosity) (n2#verbosity)
  method echo vn str = if vn<=self#verbosity then print_string str
  method echoln vn str = if vn<=self#verbosity then print_endline str
  
  (* module *)
  method moduleDef m rs = (n1#moduleDef m (List.map fst rs), n2#moduleDef m (List.map snd rs))
  
  (* definition *)
  method definition v m d r = (n1#definition (fst v) m d (fst r), n2#definition (snd v) m d (snd r))
  
  (* process *)
  method choice v m d p rs = (n1#choice (fst v) m d p (List.map fst rs), n2#choice (snd v) m d p (List.map snd rs))
  method branch v m d p index b g a q = (n1#branch (fst v) m d p index b (fst g) (fst a) (fst q), n2#branch (snd v) m d p index b (snd g) (snd a) (snd q))
  method call v m d p rs = (n1#call (fst v) m d p (List.map fst rs), n2#call (snd v) m d p (List.map snd rs))
  method term v m d p = (n1#term (fst v) m d p, n2#term (snd v) m d p)
  
  (* action *)
  method outAction v m d p a r = (n1#outAction (fst v) m d p a (fst r), n2#outAction (snd v) m d p a (snd r))
  method inAction v m d p a = (n1#inAction (fst v) m d p a, n2#inAction (snd v) m d p a)
  method tauAction v m d p a = (n1#tauAction (fst v) m d p a, n2#tauAction (snd v) m d p a)
  method newAction (v:('a*'c)) m d p a = (n1#newAction (fst v) m d p a, n2#newAction (snd v) m d p a)
  method spawnAction v m d p a rs = (n1#spawnAction (fst v) m d p a (List.map fst rs), n2#spawnAction (snd v) m d p a (List.map snd rs))
  method primAction v m d p a rs = (n1#primAction (fst v) m d p a (List.map fst rs), n2#primAction (snd v) m d p a (List.map snd rs))
  method letAction v m d p a r = (n1#letAction (fst v) m d p a (fst r), n2#letAction (snd v) m d p a (snd r))
  
  (* value *)  
  method trueValue w m d p t v = (n1#trueValue (fst w) m d p t v, n2#trueValue (snd w) m d p t v)
  method falseValue w m d p t v = (n1#falseValue (fst w) m d p t v, n2#falseValue (snd w) m d p t v)
  method intValue w m d p t v = (n1#intValue (fst w) m d p t v, n2#intValue (snd w) m d p t v)
  method stringValue w m d p t v = (n1#stringValue (fst w) m d p t v, n2#stringValue (snd w) m d p t v)
  method tupleValue w m d p t v rs = (n1#tupleValue (fst w) m d p t v (List.map fst rs), n2#tupleValue (snd w) m d p t v (List.map snd rs))
  method varValue w m d p t v = (n1#varValue (fst w) m d p t v, n2#varValue (snd w) m d p t v)
  method primValue w m d p t v rs = (n1#primValue (fst w) m d p t v (List.map fst rs), n2#primValue (snd w) m d p t v (List.map snd rs))
end

let fold_compose (n1:('a,'b) fold_node) (n2:('c,'d) fold_node) : (('a*'c),('b*'d)) fold_node = new compose_fold_node_repr n1 n2

class type iter_fold_node = object
  inherit [unit,unit] fold_node
  
  (* module *)
  method moduleDef_pre: module_type -> unit
  method moduleDef_post: module_type -> unit
  
  (* definition *)
  method definition_pre: module_type -> definition_type ->  unit
  method definition_post: module_type -> definition_type ->  unit
  
  (* process *)
  method choice_pre: module_type -> definition_type -> process choice_process_type  -> unit
  method choice_post: module_type -> definition_type -> process choice_process_type  -> unit
  method branch_post: module_type -> definition_type -> process choice_process_type -> int -> process prefix_process_type ->    unit
  method branch_pre: module_type -> definition_type -> process choice_process_type -> int -> process prefix_process_type ->    unit
  method call_pre: module_type -> definition_type -> call_process_type  -> unit
  method call_post: module_type -> definition_type -> call_process_type  -> unit
  method term_pre: module_type -> definition_type -> term_process_type -> unit
  method term_post: module_type -> definition_type -> term_process_type -> unit
  
  (* action *)
  method outAction_pre: module_type -> definition_type -> process prefix_process_type -> out_action_type ->  unit
  method outAction_post: module_type -> definition_type -> process prefix_process_type -> out_action_type ->  unit
  method inAction_pre: module_type -> definition_type -> process prefix_process_type -> in_action_type -> unit
  method inAction_post: module_type -> definition_type -> process prefix_process_type -> in_action_type -> unit
  method tauAction_pre: module_type -> definition_type -> process prefix_process_type -> tau_action_type -> unit
  method tauAction_post: module_type -> definition_type -> process prefix_process_type -> tau_action_type -> unit
  method newAction_pre: module_type -> definition_type -> process prefix_process_type -> new_action_type -> unit
  method newAction_post: module_type -> definition_type -> process prefix_process_type -> new_action_type -> unit
  method spawnAction_pre: module_type -> definition_type -> process prefix_process_type -> spawn_action_type  -> unit
  method spawnAction_post: module_type -> definition_type -> process prefix_process_type -> spawn_action_type  -> unit
  method primAction_pre: module_type -> definition_type -> process prefix_process_type -> prim_action_type  -> unit
  method primAction_post: module_type -> definition_type -> process prefix_process_type -> prim_action_type  -> unit
  method letAction_pre: module_type -> definition_type -> process prefix_process_type -> let_action_type -> unit
  method letAction_post: module_type -> definition_type -> process prefix_process_type -> let_action_type -> unit
  
  (* value *)
  method trueValue_pre: module_type -> definition_type -> process_type -> Types.valueType -> bool const_value_type -> unit
  method trueValue_post: module_type -> definition_type -> process_type -> Types.valueType -> bool const_value_type -> unit
  method falseValue_pre: module_type -> definition_type -> process_type -> Types.valueType -> bool const_value_type -> unit
  method falseValue_post: module_type -> definition_type -> process_type -> Types.valueType -> bool const_value_type -> unit
  method intValue_post: module_type -> definition_type -> process_type -> Types.valueType -> int const_value_type -> unit
  method intValue_pre: module_type -> definition_type -> process_type -> Types.valueType -> int const_value_type -> unit
  method stringValue_pre: module_type -> definition_type -> process_type -> Types.valueType -> string const_value_type -> unit
  method stringValue_post: module_type -> definition_type -> process_type -> Types.valueType -> string const_value_type -> unit
  method tupleValue_pre:  module_type -> definition_type -> process_type -> Types.valueType -> value tuple_value_type  -> unit
  method tupleValue_post:  module_type -> definition_type -> process_type -> Types.valueType -> value tuple_value_type  -> unit
  method varValue_pre:  module_type -> definition_type -> process_type -> Types.valueType -> variable_type -> unit
  method varValue_post:  module_type -> definition_type -> process_type -> Types.valueType -> variable_type -> unit
  method primValue_pre:  module_type -> definition_type -> process_type -> Types.valueType -> value prim_value_type -> unit
  method primValue_post:  module_type -> definition_type -> process_type -> Types.valueType -> value prim_value_type -> unit
end

class abstract_iter_fold_node_repr (n:int) : iter_fold_node = object(self)
  
  (* config *)
  method verbosity = n
  method echo vn str = if vn<=n then print_string str
  method echoln vn str = if vn<=n then print_endline str
  
  (* module *)
  method moduleDef m rs = self#moduleDef_post m
  method moduleDef_val m = ()
  method moduleDef_pre m = ()
  method moduleDef_post m = ()
  
  (* definition *)
  method definition v m d r = self#definition_post m d
  method definition_val v m d = ()
  method definition_pre m d = ()
  method definition_post m d = ()
  
  (* process *)
  method choice v m d p rs = self#choice_post m d p
  method choice_val v m d p = ()
  method choice_pre m d p = ()
  method choice_post m d p = ()
  method branch v m d p index b g a q = self#branch_post m d p index b
  method branch_val v m d p index b = ()
  method branch_pre m d p index b = ()
  method branch_post m d p index b = ()
  method call v m d p rs = self#call_post m d p
  method call_val v m d p = ()
  method call_pre m d p = ()
  method call_post m d p = ()
  method term v m d p = self#term_post m d p
  method term_val v m d p = ()
  method term_pre m d p = ()
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
  method tauAction v m d p a = self#tauAction_post m d p a
  method tauAction_val v m d p a = ()
  method tauAction_pre m d p a = ()
  method tauAction_post m d p a = ()
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
  
  (* value *)  
  method trueValue w m d p t v = self#trueValue_post m d p t v
  method trueValue_val w m d p t v = ()
  method trueValue_pre m d p t v = ()
  method trueValue_post m d p t v = ()
  method falseValue w m d p t v = self#falseValue_post m d p t v
  method falseValue_val w m d p t v = ()
  method falseValue_pre m d p t v = ()
  method falseValue_post m d p t v = ()
  method intValue w m d p t v = self#intValue_post m d p t v
  method intValue_val w m d p t v = ()
  method intValue_pre m d p t v = ()
  method intValue_post m d p t v = ()
  method stringValue w m d p t v = self#stringValue_post m d p t v
  method stringValue_val w m d p t v = ()
  method stringValue_pre m d p t v = ()
  method stringValue_post m d p t v = ()
  method tupleValue w m d p t v vs = self#tupleValue_post m d p t v
  method tupleValue_val w m d p t v = ()
  method tupleValue_pre m d p t v = ()
  method tupleValue_post m d p t v = ()
  method varValue w m d p t v = self#varValue_post m d p t v
  method varValue_val w m d p t v = ()
  method varValue_pre m d p t v = ()
  method varValue_post m d p t v = ()
  method primValue w m d p t v vs = self#primValue_post m d p t v
  method primValue_val w m d p t v = ()
  method primValue_pre m d p t v = ()
  method primValue_post m d p t v = ()
end

(** Take two parameters :
    n1:iter_fold_node
    n2:('a,'b) fold_node
    
    for each method m arg_1 ... arg_n will apply the sequence 
    n1#m arg_1 ... arg_n; n2#arg_1 ... arg_n
*)
class ['a,'b] seq_fold_node_repr (n1:iter_fold_node) (n2:('a,'b) fold_node) : ['a,'b] fold_node = 
object(self)
  
  (* config *)
  method verbosity = max (n1#verbosity) (n2#verbosity)
  method echo vn str = if vn<=self#verbosity then print_string str
  method echoln vn str = if vn<=self#verbosity then print_endline str
  
  (* module *)
  method moduleDef_val m = n1#moduleDef_val m ; n2#moduleDef_val m
  method moduleDef m rs = n1#moduleDef m [] ; n2#moduleDef m rs
  
  (* definition *)
  method definition_val v m d = n1#definition_val () m d ; n2#definition_val v m d
  method definition v m d r = n1#definition () m d () ; n2#definition v m d r
  
  (* process *)
  method choice_val v m d p = n1#choice_val () m d p ; n2#choice_val v m d p
  method choice v m d p rs = n1#choice () m d p [] ; n2#choice v m d p rs
  method branch_val v m d p index b = n1#branch_val () m d p index b ; n2#branch_val v m d p index b
  method branch v m d p index b g a q = n1#branch () m d p index b () () () ; n2#branch v m d p index b g a q
  method call_val v m d p = n1#call_val () m d p ; n2#call_val v m d p
  method call v m d p rs = n1#call () m d p [] ; n2#call v m d p rs
  method term_val v m d p = n1#term_val () m d p ; n2#term_val v m d p
  method term v m d p = n1#term () m d p ; n2#term v m d p
  
  (* action *)
  method outAction_val v m d p a = n1#outAction_val () m d p a ; n2#outAction_val v m d p a
  method outAction v m d p a r = n1#outAction () m d p a () ; n2#outAction v m d p a r
  method inAction_val v m d p a = n1#inAction_val () m d p a ; n2#inAction_val v m d p a
  method inAction v m d p a = n1#inAction () m d p a ; n2#inAction v m d p a
  method tauAction_val v m d p a = n1#tauAction_val () m d p a ; n2#tauAction_val v m d p a
  method tauAction v m d p a = n1#tauAction () m d p a ; n2#tauAction v m d p a
  method newAction_val v m d p a = n1#newAction_val () m d p a ; n2#newAction_val v m d p a
  method newAction v m d p a = n1#newAction () m d p a ; n2#newAction v m d p a
  method spawnAction_val v m d p a = n1#spawnAction_val () m d p a ; n2#spawnAction_val v m d p a
  method spawnAction v m d p a rs = n1#spawnAction () m d p a [] ; n2#spawnAction v m d p a rs
  method primAction_val v m d p a = n1#primAction_val () m d p a ; n2#primAction_val v m d p a 
  method primAction v m d p a rs = n1#primAction () m d p a [] ; n2#primAction v m d p a rs
  method letAction_val v m d p a = n1#letAction_val () m d p a ; n2#letAction_val v m d p a
  method letAction v m d p a r = n1#letAction () m d p a () ; n2#letAction v m d p a r
  
  (* value *)  
  method trueValue_val w m d p t v = n1#trueValue_val () m d p t v ; n2#trueValue_val w m d p t v
  method trueValue w m d p t v = n1#trueValue () m d p t v ; n2#trueValue w m d p t v
  method falseValue_val w m d p t v = n1#falseValue_val () m d p t v ; n2#falseValue_val w m d p t v
  method falseValue w m d p t v = n1#falseValue () m d p t v ; n2#falseValue w m d p t v
  method intValue_val w m d p t v = n1#intValue_val () m d p t v ; n2#intValue_val w m d p t v
  method intValue w m d p t v = n1#intValue () m d p t v ; n2#intValue w m d p t v
  method stringValue_val w m d p t v = n1#stringValue_val () m d p t v ; n2#stringValue_val w m d p t v
  method stringValue w m d p t v = n1#stringValue () m d p t v ; n2#stringValue w m d p t v
  method tupleValue_val w m d p t v = n1#tupleValue_val () m d p t v ; n2#tupleValue_val w m d p t v
  method tupleValue w m d p t v rs = n1#tupleValue () m d p t v [] ; n2#tupleValue w m d p t v rs
  method varValue_val w m d p t v = n1#varValue_val () m d p t v ; n2#varValue_val w m d p t v
  method varValue w m d p t v = n1#varValue () m d p t v ; n2#varValue w m d p t v
  method primValue_val w m d p t v = n1#primValue_val () m d p t v ; n2#primValue_val w m d p t v
  method primValue w m d p t v rs = n1#primValue () m d p t v [] ; n2#primValue w m d p t v rs
end

let fold_seq (n1:iter_fold_node) (n2:('a,'b) fold_node) = new seq_fold_node_repr n1 n2

let rec fold_seq_all ns n = match ns with
  | [] -> n
  | n'::ns' -> fold_seq n' (fold_seq_all ns' n)

(** fold_node "handler", apply the fold_node methods on a moduleDef and his definition list *)
let rec fold_module (m:moduleDef) (n:('a,'b) fold_node) : 'b =
  match m with
    | Module m' -> 
      n#moduleDef m' (List.fold_left (fun ds d -> (definition_fold (n#moduleDef_val m') m' d n)::ds) [] m'#definitions)
	(* n#moduleDef: module_type -> 'b list -> 'b *)
	
and definition_fold (v:'a) (m:module_type) (def:definition) (n:('a,'b) fold_node) =
  match def with
    | Def d ->
      n#definition v m d (process_fold (n#definition_val v m d) m d d#process n)
	(* n#definition: 'a -> module_type -> definition_type -> ('a, 'b) -> ('a, 'b) *)

and process_fold (v:'a) (m:module_type) (d:definition_type) (proc:process) (n:('a,'b) fold_node) =
  match proc with
    | Term p -> term_process_fold v m d p n
    | Call p -> call_process_fold v m d p n
    | Choice p -> choice_process_fold v m d p n

and term_process_fold (v:'a) (m:module_type) (d:definition_type) (p:term_process_type) (n:('a,'b) fold_node) =
  n#term_val v m d p ; n#term v m d p

and call_process_fold (v:'a) (m:module_type) (d:definition_type) (p:call_process_type) (n:('a,'b) fold_node) =
  n#call v m d p (List.fold_left (fun vs (t',v') -> (value_fold (n#call_val v m d p) m d (p:>process_type) t' v' n)::vs) [] (List.combine p#argTypes p#args))

and choice_process_fold (v:'a) (m:module_type) (d:definition_type) (p:process choice_process_type) (n:('a,'b) fold_node) =
  n#choice v m d p (list_fold_n (fun bs index branch -> (branch_process_fold (n#choice_val v m d p) m d p index branch n)::bs) [] p#branches)

and branch_process_fold (v:'a) (m:module_type) (d:definition_type) (c:process choice_process_type) (index:int) (p:process prefix_process_type) (n:('a,'b) fold_node) =
  let w = n#branch_val v m d c index p in
  n#branch v m d c index p (value_fold w m d (p:>process_type) p#guardType p#guard n) (action_fold w m d p p#action n) (process_fold w m d p#continuation n)

and action_fold (v:'a) (m:module_type) (d:definition_type) (p:process prefix_process_type) (act:action) (n:('a,'b) fold_node) =
  match act with
    | Tau a -> tau_action_fold v m d p a n
    | Output a -> out_action_fold v m d p a n
    | Input a -> in_action_fold v m d p a n
    | New a -> new_action_fold v m d p a n
    | Spawn a -> spawn_action_fold v m d p a n
    | Prim a ->  prim_action_fold v m d p a n
    | Let a -> let_action_fold v m d p a n
and tau_action_fold (v:'a) (m:module_type) (d:definition_type) (p:process prefix_process_type) (a:tau_action_type) (n:('a,'b) fold_node) =
  n#tauAction_val v m d p a ; n#tauAction v m d p a
and out_action_fold (v:'a) (m:module_type) (d:definition_type) (p:process prefix_process_type) (a:out_action_type) (n:('a,'b) fold_node) =
  n#outAction v m d p a (value_fold (n#outAction_val v m d p a) m d (p:>process_type) a#valueType a#value n)
and in_action_fold (v:'a) (m:module_type) (d:definition_type) (p:process prefix_process_type) (a:in_action_type) (n:('a,'b) fold_node) =
  n#inAction_val v m d p a ; n#inAction v m d p a
and new_action_fold (v:'a) (m:module_type) (d:definition_type) (p:process prefix_process_type) (a:new_action_type) (n:('a,'b) fold_node) =
  n#newAction_val v m d p a ; n#newAction v m d p a
and spawn_action_fold (v:'a) (m:module_type) (d:definition_type) (p:process prefix_process_type) (a:spawn_action_type) (n:('a,'b) fold_node) =
  n#spawnAction v m d p a (List.fold_left (fun vs (t',v') -> (value_fold (n#spawnAction_val v m d p a) m d (p:>process_type) t' v' n)::vs) [] (List.combine a#argTypes a#args))
and prim_action_fold (v:'a) (m:module_type) (d:definition_type) (p:process prefix_process_type) (a:prim_action_type) (n:('a,'b) fold_node) =
  n#primAction v m d p a (List.fold_left (fun vs (t',v') -> (value_fold (n#primAction_val v m d p a) m d (p:>process_type) t' v' n)::vs) [] (List.combine a#argTypes a#args))
and let_action_fold (v:'a) (m:module_type) (d:definition_type) (p:process prefix_process_type) (a:let_action_type) (n:('a,'b) fold_node) =
  n#letAction v m d p a (value_fold (n#letAction_val v m d p a) m d (p:>process_type) a#valueType a#value n)
and value_fold (w:'a) (m:module_type) (d:definition_type) (p:process_type) (t:valueType) (value:value) (n:('a,'b) fold_node) =
  match value with
    | VTrue v -> n#trueValue_val w m d p t v ; n#trueValue w m d p t v
    | VFalse v -> n#falseValue_val w m d p t v ; n#falseValue w m d p t v
    | VInt v -> n#intValue_val w m d p t v ; n#intValue w m d p t v
    | VString v -> n#stringValue_val w m d p t v ; n#stringValue w m d p t v
    | VTuple v -> tuple_value_fold w m d p t v n
    | VVar v -> n#varValue_val w m d p t v ; n#varValue w m d p t v
    | VPrim v -> prim_value_fold w m d p t v n
and tuple_value_fold (w:'a) (m:module_type) (d:definition_type) (p:process_type) (t:valueType) (v:value tuple_value_type) (n:('a,'b) fold_node) =
  n#tupleValue w m d p t v (List.fold_left (fun vs (t',v') -> (value_fold (n#tupleValue_val w m d p t v) m d (p:>process_type) t' v' n)::vs) [] (List.combine v#types v#elements))
and prim_value_fold (w:'a) (m:module_type) (d:definition_type) (p:process_type) (t:valueType) (v:value prim_value_type) (n:('a,'b) fold_node) =
  n#primValue w m d p t v (List.fold_left (fun vs (t',v') -> (value_fold (n#primValue_val w m d p t v) m d (p:>process_type) t' v' n)::vs) [] (List.combine v#argTypes v#args))

let fold_module_3 (m:moduleDef) (n1:('a,'b) fold_node) (n2:('c,'d) fold_node) (n3:('e,'f) fold_node) : ('b * 'd * 'f) =
  let (r1, (r2,r3)) = fold_module m (fold_compose n1 (fold_compose n2 n3))
  in (r1,r2,r3)

let fold_module_4 m n1 n2 n3 n4 =
  let ((r1,r2), (r3,r4)) = fold_module m (fold_compose (fold_compose n1 n2) (fold_compose n3 n4))
  in (r1,r2,r3,r4)

let fold_module_iter_all m ns n =
  fold_module m (fold_seq_all ns n)



