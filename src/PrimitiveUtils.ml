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

let declare_primitive table module_name def_name =
  Hashtbl.add table module_name def_name
;;
 
let prim_type_table : (k, v) Hashtbl.t = Hashtbl.create 10;;

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
    ("corearith", "compare")
    (makePrimTypeRepr "core/arith" "compare" [TInt; TInt] TBool);
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

  (* (\** The hash table representing the primitive procedures *\) *)
  (* let proc_table : (k, v) Hashtbl.t = Hashtbl.create 10;; *)

  (** The hash table representing the primitive focntions *)
  let prim_name_table : (k, v) Hashtbl.t = Hashtbl.create 10;;
  
  (** Add a primitive into a table 
  let declare_primitive table module_name def_name =
    Hashtbl.add table module_name def_name
  ;;*)
  
  (** get the value of a primitive *)
  let get_value_name module_name def_name =
    try
let name = Hashtbl.find prim_name_table (module_name, def_name) in
	name
    with _ -> failwith "primitive name unknown"
  ;;
  
  (** Initialization with all the wanted primitives *)
  let _ =
    declare_primitive prim_name_table ("corearith", "add") Prims.add_name;

    declare_primitive prim_name_table ("corearith", "substract") Prims.substract_name;

    declare_primitive prim_name_table ("corearith", "compare") Prims.compare_name; 

    declare_primitive prim_name_table ("coreio", "print_info") Prims.print_info_name;

    declare_primitive prim_name_table ("coreio", "print_str") Prims.print_str_name; 

    declare_primitive prim_name_table ("coreio", "print_int") Prims.print_int_name
  ;;

end
