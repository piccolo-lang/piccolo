(* module TypeRepr
   ---------------

  Representation of types

*)
(** This module defines representation of types interfaces defined in Types. *)

open Utils;; 
open Types;;

(** implementation of Types.tuple_type with a list of valueType*)
class tuple_type_repr (ts : valueType list) =
object
  method arity = List.length ts 
  method elements = ts
  method toString = string_of_collection "" "" "*" string_of_valueType ts
end
  
(** constructor tuple_type_repr *)
let makeTupleTypeRepr ts = new tuple_type_repr ts;;

(** return a TTuple of type valueType *)
let makeTupleType : valueType list -> valueType = 
  function ts -> TTuple (makeTupleTypeRepr ts) 
;;

(** implementation of Types.prim_type *)
class prim_type_repr (mname : string) (pname : string) (ps : valueType list) (rt : valueType) = object
  method arity = List.length ps
  method params = ps
  method return = rt
  method moduleName = mname 
  method primName = pname
  method toString = (string_of_collection "" "" "*" string_of_valueType ps) ^ "->" ^ (string_of_valueType rt)
end
  
(** constructor prim_type_repr *)
let makePrimTypeRepr mname pname ps rt = new prim_type_repr mname pname ps rt;;
  
(** instantiation of a TPrim of type valueType *)
let makePrimType mname pname ps rt = TPrim (makePrimTypeRepr mname pname ps rt);;
