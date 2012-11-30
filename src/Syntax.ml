
(* module Syntax *)

open Utils ;;
open Types ;;

class type virtual ast_type = object
  method startLine : int
  method startChar : int
  method endChar : int
  method endLine : int
  method posString : string
  method virtual toString : string
end

class type virtual ast_binder_type = object
  inherit ast_type
  method fetchBinderType: string -> valueType option
end
            
(* values *)
                  
class type virtual value_type = object
  inherit ast_type
  method ofType : valueType
  method setType : valueType -> unit
end

class type ['a] const_value_type = object
  inherit value_type
  method toVal : 'a
  method toString : string
end

class type ['a] tuple_value_type = object
  inherit value_type
  method arity : int
  method types : valueType list
  method elements : 'a list
  method toString : string
end
          
class type variable_type = object
  inherit value_type
  method name : string
  method index : int
  method setIndex : int -> unit
  method binder : ast_binder_type option
  method setBinder : ast_binder_type -> unit
  method toString : string
end
      
class type ['a] prim_value_type = object
  inherit value_type
  method moduleName : string
  method primName : string
  method arity : int
  method args : 'a list
  method argTypes : valueType list
  method toString : string
end

type value =
  | VTrue of bool const_value_type
  | VFalse of bool const_value_type
  | VInt of int const_value_type
  | VString of string const_value_type
  | VTuple of value tuple_value_type
  | VVar of variable_type
  | VPrim of value prim_value_type  (* primitives *)

let value_type_of_value : (value -> value_type) = function
  | VTrue v -> (v :> value_type)
  | VFalse v -> (v :> value_type)
  | VInt v -> (v :> value_type)
  | VString v -> (v :> value_type)
  | VTuple v -> (v :> value_type)
  | VVar v -> (v :> value_type)
  | VPrim v -> (v :> value_type)

let string_of_value v = (value_type_of_value v)#toString 

(* actions *)

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

class type tau_action_type = object
  inherit ast_type
  method toString : string
end
    
class type new_action_type = object
  inherit ast_binder_type
  method variable : string
  method variableType : valueType
  method variableIndex : int
  method setVariableIndex : int -> unit
  method fetchBinderType : string -> valueType option
  method toString : string
end

class type spawn_action_type = object
  inherit ast_type
  method modName : string
  method defName : string
  method arity : int
  method args : value list
  method argTypes : valueType list
  method toString : string
end

class type prim_action_type = object
  inherit ast_type
  method moduleName : string
  method primName : string
  method arity : int
  method args : value list
  method argTypes : valueType list
  method toString : string
end

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

type action = 
  | Tau of tau_action_type
  | Output of out_action_type
  | Input of in_action_type
  | New of new_action_type
  | Spawn of spawn_action_type
  | Prim of prim_action_type
  | Let of let_action_type

let ast_type_of_action : (action -> ast_type) = function
  | Tau a -> (a :> ast_type)
  | Output a -> (a :> ast_type)
  | Input a -> (a :> ast_type)
  | New a -> (a :> ast_type)
  | Spawn a -> (a :> ast_type)
  | Prim a ->  (a :> ast_type)
  | Let a -> (a :> ast_type)

let string_of_action a = (ast_type_of_action a)#toString

(* processes *)

class type virtual process_type = object
  inherit ast_type
  method inModule : string
  method inDef : string
end

class type term_process_type = object
  inherit process_type
  method toString : string
end

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

(* 'a = process ? find another instanciation of 'a *)
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

class type ['a] choice_process_type = object
  inherit process_type
  method branches : ('a prefix_process_type) list
  method arity : int
  method toString : string
end

type process = 
  | Term  of term_process_type (* termination *)
  | Call of call_process_type
  | Choice of process choice_process_type 
      
let process_type_of_process = function
  | Term p -> (p :> process_type)
  | Call p -> (p :> process_type)
  | Choice p -> (p :> process_type)

let string_of_process p = (process_type_of_process p)#toString

(* definitions *)

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

type definition =
  | Def of definition_type

let definition_type_of_definition = function
  | Def d -> d

let string_of_definition = function
  | Def d -> d#toString
    
(* modules *)

class type module_type = object
  inherit ast_type
  method name : string
  method definitions : definition list
  method addDefinition : definition -> unit
  method lookupDef : string -> definition
  method toString : string
end

type moduleDef =
  | Module of module_type

let module_type_of_module = function
  | Module m -> m

let string_of_module = function
  | Module m -> m#toString

