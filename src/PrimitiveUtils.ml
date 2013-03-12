(* module PrimitiveUtils 
   --------------

   Handle Primitives

*)
(** This module defines various utilities functions to handle Primitive usage. *)

open Types;;
open TypeRepr;;
open SeqAST;;

(** The type of the key, represents the name of the primitive *)
type k = string * string;;
(** The type of the value, represents the primitive with name, argument types, return type *)
type v = string * prim_type_repr;;

(** The hash table representing the primitive procedures *)
let proc_table : (k, v) Hashtbl.t = Hashtbl.create 10;;

(** The hash table representing the primitive focntions *)
let value_table : (k, v) Hashtbl.t = Hashtbl.create 10;;

(** Add a primitive into a table *)
let declare_primitive table module_name def_name =
  Hashtbl.add table module_name def_name
;;

(** get the value of a primitive *)
let get_value_name module_name def_name =
  try
    let (name, _) = Hashtbl.find value_table (module_name, def_name) in
      name
  with _ -> failwith "primitive name unknown"
;;

(** Initialization with all the wanted primitives *)
let init () =
  declare_primitive
    value_table 
    ("corearith", "add") 
    ("PICC_Int_add", (makePrimTypeRepr "core/arith/int" "add" [TInt; TInt] TInt));
  declare_primitive 
    value_table
    ("corearith", "substract") 
    ("PICC_Int_substract", (makePrimTypeRepr "core/arith/int" "substract" [TInt; TInt] TInt));
  declare_primitive
    value_table
    ("corearith", "compare") 
    ("compare_values", (makePrimTypeRepr "core/arith/int" "compare" [TInt; TInt] TInt));
  declare_primitive
    value_table
    ("coreio", "print_info") 
    ("PICC_print_value_infos", (makePrimTypeRepr "core/io" "print_info" [TString] TString));
  declare_primitive
    value_table
    ("coreio", "print") 
    ("PICC_print_value", (makePrimTypeRepr "core/io" "print" [TString] TString))
;;

init();;
