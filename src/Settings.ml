(* Prepare for parsing the command line. *)

let filename = ref None
let insert name = filename := Some name

let verbose = ref 0
let outname = ref "out.c"

let options = Arg.align [
  "-v", Arg.Unit (fun () -> incr verbose), "increase verbosity";
  "-o", Arg.Set_string outname, "set the output file name (out.c by default)";
]

let usage =
  Printf.sprintf "Usage: %s <options> <pithread filename>" Sys.argv.(0)

(* Parse the command line. *)

let () = Arg.parse options insert usage

(* Export the settings. *)

let filename =
  match !filename with
  | None ->
      Arg.usage options usage;
      exit 1
  | Some filename ->
      filename

let verbose = !verbose
let outname = !outname
