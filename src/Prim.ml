
open Types
open SeqAST

type k = string * string
type v = string * ((valueType list) * valueType)

let proc_table : (k, v) Hashtbl.t = Hashtbl.create 2
let value_table : (k, v) Hashtbl.t = Hashtbl.create 2

let _ =
  Hashtbl.add value_table ("core/arith", "add") ("PICC_Int_add", ([TInt; TInt], TInt))

let get_value_name module_name def_name =
  let (name, _) = Hashtbl.find value_table (module_name,def_name) in
  name
