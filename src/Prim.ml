
open Types
open TypeRepr
open SeqAST

type k = string * string
type v = string * prim_type_repr

let proc_table : (k, v) Hashtbl.t = Hashtbl.create 2
let value_table : (k, v) Hashtbl.t = Hashtbl.create 2

let _ =
  Hashtbl.add value_table 
    ("corearith", "add") 
    ("PICC_Int_add", (makePrimTypeRepr "core/arith/int" "add" [TInt; TInt] TInt));
  Hashtbl.add value_table 
    ("corearith", "substract") 
    ("PICC_Int_substract", (makePrimTypeRepr "core/arith/int" "substract" [TInt; TInt] TInt));
  Hashtbl.add value_table 
    ("corearith", "compare") 
    ("compare_values", (makePrimTypeRepr "core/arith/int" "compare" [TInt; TInt] TInt));
  Hashtbl.add value_table ("coreio", "print") 
    ("PICC_print_value_infos", (makePrimTypeRepr "core/io" "print" [TString] TString));;

let get_value_name module_name def_name =
  try
    let (name, _) = Hashtbl.find value_table (module_name, def_name) in
      name
  with _ -> failwith "primitive name unknown"
