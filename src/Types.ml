(* module Types
   -----------

  Type system

*)
(** This module defines various utility functions used in other parts of the compiler. *)

open Utils;;

(** interface representing a tuple of a given type *)
class type ['a] tuple_type = object
  method arity : int
  method elements : 'a list
  method toString : string
end

(** interface representing a primitive type *)
class type ['a,'b] prim_type = object
  method arity : int
  method params : 'a
  method return : 'b
  method moduleName : string
  method primName : string
  method toString : string
end

(** type representing valueType *)
type valueType =
  | TUnknown
  | TBool 
  | TInt
  | TString
  | TTuple of valueType tuple_type 
  | TChan of valueType
  | TPrim of ((valueType list), valueType) prim_type (* primitive type *)

(** string representing a valueType *)
let rec string_of_valueType = function
  | TUnknown -> "unknown"
  | TBool -> "bool"
  | TInt -> "int"
  | TString -> "string"
  | TTuple t -> t#toString
  | TChan t -> "chan<" ^ (string_of_valueType t) ^ ">"
  | TPrim t -> "prim[" ^ (t#moduleName) ^ ":" ^ (t#primName) ^ "]:" ^ (t#toString)

(** exception Ex_Type_Non_WellFormed of valueType *)
exception Ex_Type_Non_WellFormed of valueType ;;

(** test if a valueType is well formed *)
let rec checkTypeWellFormed u = 
  let rec check t inPrim = match t with
    | TPrim t' -> 
      if inPrim then raise (Ex_Type_Non_WellFormed t)
      else (List.for_all (fun t'' -> check t'' true) (t'#params))
        && check (t'#return) true
    | TTuple t' -> List.for_all (fun t'' -> check t'' inPrim) (t'#elements)
    | TChan t' -> check t' inPrim
    | _ -> true
  in check u false

(** test if two given types are equals *)
let rec type_eq t1 t2 = match (t1,t2) with
  | (TUnknown,TUnknown) -> true
  | (TBool, TBool) -> true
  | (TInt, TInt) -> true
  | (TString, TString) -> true
  | (TTuple t1, TTuple t2) -> t1#arity = t2#arity && List.for_all2 type_eq t1#elements t2#elements
  | (TChan t1, TChan t2) -> type_eq t1 t2
  | (TPrim t1, TPrim t2) -> t1#arity = t2#arity && (List.for_all2 type_eq t1#params t2#params) && (type_eq t1#return t2#return)
  | (_,_) -> false

