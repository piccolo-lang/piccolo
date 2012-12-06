(* module Syntax
   -----------

  Syntax
   See Parallele Computing with the Pi-Calculus, page 2 Table 1
*)
(** This module defines the syntax of the compiler. *)

open Utils;;
open Types;;

(** interface representing the type of an abstract syntax tree *)
class type virtual ast_type = object
  method startLine : int
  method startChar : int
  method endChar : int
  method endLine : int
  method posString : string
  method virtual toString : string
end
	  
(** interface representing a bound ast with a valueType, extends ast_type *)
class type virtual ast_binder_type = object
  inherit ast_type
  method fetchBinderType: string -> valueType option
end
            
(* values *)
                  
(** interface representing a valueType, extends ast_type *) 
class type virtual value_type = object
  inherit ast_type
  method ofType : valueType
  method setType : valueType -> unit
end

(** interface representing a constant valueType, extends value_type *)
class type ['a] const_value_type = object
  inherit value_type
  method toVal : 'a
  method toString : string
end

(** interface representing a tuple valueType, extends value_type *)
class type ['a] tuple_value_type = object
  inherit value_type
  method arity : int
  method types : valueType list
  method elements : 'a list
  method toString : string
end
          
(** interface representing a variable type, extends value_type *)
class type variable_type = object
  inherit value_type
  method name : string
  method index : int
  method setIndex : int -> unit
  method binder : ast_binder_type option
  method setBinder : ast_binder_type -> unit
  method toString : string
end
	  
(** interface reprsenting a primitive valueType, extends value_type *)
class type ['a] prim_value_type = object
  inherit value_type
  method moduleName : string
  method primName : string
  method arity : int
  method args : 'a list
  method argTypes : valueType list
  method toString : string
end
	  
(** type representing a value *)
type value =
  | VTrue of bool const_value_type
  | VFalse of bool const_value_type
  | VInt of int const_value_type
  | VString of string const_value_type
  | VTuple of value tuple_value_type
  | VVar of variable_type
  | VPrim of value prim_value_type  (* primitives *)
      
(** value representing a valueType *)
let value_type_of_value : (value -> value_type) = function
  | VTrue v -> (v :> value_type)
  | VFalse v -> (v :> value_type)
  | VInt v -> (v :> value_type)
  | VString v -> (v :> value_type)
  | VTuple v -> (v :> value_type)
  | VVar v -> (v :> value_type)
  | VPrim v -> (v :> value_type)
      
(** string representing a value *)
let string_of_value v = (value_type_of_value v)#toString 
  
(* actions *)
  
(** interface representing the type of an output! action, extends ast_type *)
class type out_action_type = object
  inherit ast_type
  method channel : string
  method channelType : valueType
  method channelIndex : int
  method channelBinder : ast_binder_type option
  method setChannelBinder : ast_binder_type -> unit
  method setChannelIndex : int -> unit
  method value : value
  method valueType : valueType
  method toString : string
end

(** interface representing the type of an input? action, extends ast_binder_type *)
class type in_action_type = object
  inherit ast_binder_type
  method channel : string
  method channelType : valueType
  method channelIndex : int
  method setChannelIndex : int -> unit
  method channelBinder : ast_binder_type option
  method setChannelBinder : ast_binder_type -> unit
  method variable : string
  method variableType : valueType
  method variableIndex : int
  method fetchBinderType : string -> valueType option
  method setVariableIndex : int -> unit
  method toString : string
end

(** interface representing the type of the silent action tau, extends ast_type *)
class type tau_action_type = object
  inherit ast_type
  method toString : string
end

(** interface representing the type of the channel creation new, extends ast_binder_type *)    
class type new_action_type = object
  inherit ast_binder_type
  method variable : string
  method variableType : valueType
  method variableIndex : int
  method setVariableIndex : int -> unit
  method fetchBinderType : string -> valueType option
  method toString : string
end

(** interface representing the type of the thread creation spawn, extends ast_type *)
class type spawn_action_type = object
  inherit ast_type
  method modName : string
  method defName : string
  method arity : int
  method args : value list
  method argTypes : valueType list
  method toString : string
end

(** interface representing the type of a primitive action, extends ast_type *)
class type prim_action_type = object
  inherit ast_type
  method moduleName : string
  method primName : string
  method arity : int
  method args : value list
  method argTypes : valueType list
  method toString : string
end
	  
(** interface representing the type of the action let, extends ast_binder_type *)
class type let_action_type = object
  inherit ast_binder_type
  method variable : string
  method variableType : valueType
  method variableIndex : int
  method setVariableIndex : int -> unit
  method fetchBinderType : string -> valueType option
  method value : value
  method valueType : valueType
  method toString : string
end

(** type representing action *)
type action = 
  | Tau of tau_action_type
  | Output of out_action_type
  | Input of in_action_type
  | New of new_action_type
  | Spawn of spawn_action_type
  | Prim of prim_action_type
  | Let of let_action_type

(** action representing an ast_type *)
let ast_type_of_action : (action -> ast_type) = function
  | Tau a -> (a :> ast_type)
  | Output a -> (a :> ast_type)
  | Input a -> (a :> ast_type)
  | New a -> (a :> ast_type)
  | Spawn a -> (a :> ast_type)
  | Prim a ->  (a :> ast_type)
  | Let a -> (a :> ast_type)

(** string representing an action *)
let string_of_action a = (ast_type_of_action a)#toString

(* processes *)

(** interface representing the type of a process, extends ast_type *)
class type virtual process_type = object
  inherit ast_type
  method inModule : string
  method inDef : string
end

(** interface representing the type of a process termination end, extends process_type *)
class type term_process_type = object
  inherit process_type
  method toString : string
end

(** interface representing the type of a process Call, extends process_type *)
class type call_process_type = object
  inherit process_type
  method moduleName : string
  method defName : string
  method args : value list
  method arity : int
  method argTypes : valueType list
  method setArgTypes : valueType list -> unit
  method toString : string
end

<<<<<<< HEAD
(** interface representing the type of a prefix process, extends process_type *)
=======
(* 'a = process ? find another instanciation of 'a *)
>>>>>>> 71873978b8b6f80813a76046f9bbde9653e49faf
class type ['a] prefix_process_type = object
  inherit process_type
  method guard : value
  method setGuardType: valueType -> unit
  method guardType : valueType
  method action : action
  method continuation : 'a
  method index : int
  method toString : string
end

(** interface representing the type of a process Choice, extends process_type *)
class type ['a] choice_process_type = object
  inherit process_type
  method branches : ('a prefix_process_type) list
  method arity : int
  method toString : string
end

(** type representing a process *)
type process = 
  | Term  of term_process_type (* termination *)
  | Call of call_process_type
  | Choice of process choice_process_type 
      
(** process_type representing a process *)
let process_type_of_process = function
  | Term p -> (p :> process_type)
  | Call p -> (p :> process_type)
  | Choice p -> (p :> process_type)

(** string representing a process *)
let string_of_process p = (process_type_of_process p)#toString

(* definitions *)

(** interface representing the type of a definition, extends ast_binder_type *)
class type definition_type = object
  inherit ast_binder_type
  method name : string
  method params : (string * valueType) list
  method arity : int
  method setParamTypes: valueType list -> unit
  method env : string list
  method extendEnv : string -> unit
  method lookupEnv : string -> int option
  method fetchBinderType : string -> valueType option
  method esize : int
  method csize : int
  method process : process
  method toString : string
end

(** type representing a definition *)
type definition =
  | Def of definition_type

(** definition_type of a definition *)
let definition_type_of_definition = function
  | Def d -> d

(** string representing a definitnion *)
let string_of_definition = function
  | Def d -> d#toString
    
(* modules *)

(** interface representing the type of a module *)
class type module_type = object
  inherit ast_type
  method name : string
  method definitions : definition list
  method addDefinition : definition -> unit
  method lookupDef : string -> definition
  method toString : string
end

(** type representing de definition of a module *)
type moduleDef =
  | Module of module_type

(** module_type of a module *)
let module_type_of_module = function
  | Module m -> m

(** string representing a given module *)
let string_of_module = function
  | Module m -> m#toString

