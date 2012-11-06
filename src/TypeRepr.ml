
(* module TypeRepr
   ---------------

  Representation of types

*)

open Utils;;
open Types;;

class tuple_type_repr (ts: valueType list) = object
  method arity = List.length ts
  method elements = ts
  method toString = string_of_collection "" "" "*" string_of_valueType ts
end

let makeTupleTypeRepr ts = new tuple_type_repr ts

let makeTupleType : valueType list -> valueType = 
  function ts -> TTuple (makeTupleTypeRepr ts)
  

class prim_type_repr (mname:string) (pname:string) (ps : valueType list) (rt : valueType) = object
  method moduleName = mname
  method primName = pname
  method arity = List.length ps
  method params = ps
  method return = rt
  method toString = (string_of_collection "" "" "*" string_of_valueType ps) ^ "->" ^ (string_of_valueType rt)
end
  
let makePrimTypeRepr mname pname ps rt = new prim_type_repr mname pname ps rt

let makePrimType mname pname ps rt = TPrim (makePrimTypeRepr mname pname ps rt)
