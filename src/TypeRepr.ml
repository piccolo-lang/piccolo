
(* module TypeRepr
   ---------------

  Representation of types

*)

(** This module defines the implementation of the interfaces defined in Types *)

open Utils;; 
open Types;;

(** Implementation of tuple_type with a valueType*)
class tuple_type_repr (ts: valueType list) = object
  method arity = List.length ts 
  method elements = ts
  method toString = string_of_collection "" "" "*" string_of_valueType ts
end

(** Instantiation of a tuple_type_repr *)
let makeTupleTypeRepr ts = new tuple_type_repr ts

(** Return a TTuple of type valueType *)
let makeTupleType : valueType list -> valueType = 
  function ts -> TTuple (makeTupleTypeRepr ts) 
  
(** Implementation of prim_type *)
class prim_type_repr (mname:string) (pname:string) (ps : valueType list) (rt : valueType) = object
  method moduleName = mname 
  method primName = pname
  method arity = List.length ps
  method params = ps
  method return = rt
  method toString = (string_of_collection "" "" "*" string_of_valueType ps) ^ "->" ^ (string_of_valueType rt)
end
  
(** Instantiation of a prim_type_repr with  *)
let makePrimTypeRepr mname pname ps rt = new prim_type_repr mname pname ps rt

(** Return a TPrim of type valueType *)
let makePrimType mname pname ps rt = TPrim (makePrimTypeRepr mname pname ps rt)
