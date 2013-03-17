(* module PrimitiveUtils 
   --------------

   Handle Primitives
   
*)
(** This module defines the functor to handle Primitive usage. *)

open Types;;
open TypeRepr;;
open SeqAST;;

(** The type of the key, represents the name of the primitive *)
type k = string * string;;

(** The type of the value, represents the primitive with name, argument types, return type *)
type v = prim_type_repr;;

(** Add a primitive into a chosen table *)
let declare_primitive table module_name def_name =
  Hashtbl.add table module_name def_name
;;
 
(** The hash table representing the types of primitive focntions *)
let prim_type_table : (k, v) Hashtbl.t = Hashtbl.create 10;;

(** get the value of a primitive *)
let get_value_type module_name def_name =
  try
    Hashtbl.find prim_type_table (module_name, def_name)
  with _ -> failwith "primitive name unknown"
;;

(* Initialization of type table with all the wanted primitives *)
let _ =
  declare_primitive
    prim_type_table 
    ("corearith", "add")
    (makePrimTypeRepr "core/arith" "add" [TInt; TInt] TInt);
  declare_primitive
    prim_type_table
    ("corearith", "substract")
    (makePrimTypeRepr "core/arith" "substract" [TInt; TInt] TInt);
  declare_primitive
    prim_type_table
    ("corearith", "equals")
    (makePrimTypeRepr "core/arith" "equals" [TInt; TInt] TBool);
  declare_primitive
    prim_type_table
    ("coreio", "print_info")
    (makePrimTypeRepr "core/io" "print_info" [TString] TString);
  declare_primitive
    prim_type_table
    ("coreio", "print_str")
    (makePrimTypeRepr "core/io" "print_str" [TString] TString);
  declare_primitive
    prim_type_table
    ("coreio", "print_int")
    (makePrimTypeRepr "core/io" "print_int" [TInt] TString);;
    
module Make (Prims : SeqAST.Prims) =
struct
  
  (** The type of the value, represents the primitive with name, argument types, return type *)
  type v = string ;;

  (** The hash table representing the names of primitive focntions *)
  let prim_name_table : (k, v) Hashtbl.t = Hashtbl.create 10;;
  
  (** get the value of a primitive *)
  let get_value_name module_name def_name =
    try
      Hashtbl.find prim_name_table (module_name, def_name)
    with _ -> failwith "primitive name unknown"
  ;;
  
  (* Initialization of name table with all the wanted primitives *)
  let _ =
    declare_primitive prim_name_table ("corearith", "add") Prims.add_name;

    declare_primitive prim_name_table ("corearith", "substract") Prims.substract_name;

    declare_primitive prim_name_table ("corearith", "equals") Prims.equals_name; 

    declare_primitive prim_name_table ("coreio", "print_info") Prims.print_info_name;

    declare_primitive prim_name_table ("coreio", "print_str") Prims.print_str_name; 

    declare_primitive prim_name_table ("coreio", "print_int") Prims.print_int_name
  ;;

end
