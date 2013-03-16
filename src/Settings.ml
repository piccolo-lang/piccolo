(* module Settings
   -------------

  Various possible settings for the command line

*)
(** Prepare the options and filename and parse the command line. *)

let filename = ref None
let insert name = filename := Some name

let verbose = ref 5
let outname = ref "out.c"
let nb_thread = ref 4
let std_gc_fuel = ref 2
let quick_gc_fuel = ref 2
let active_factor = ref 2
let debug = ref false

let options = Arg.align [
  "-v", Arg.Unit (fun () -> incr verbose), "increase verbosity";
  "-o", Arg.Set_string outname, "set the output file name (out.c by default)";
  "-nt", Arg.Set_int nb_thread, "set the number of core threads (4 by default)";
  "-sgf", Arg.Set_int std_gc_fuel, "set the number of times the scheduler can continuasely execute a pi_thread without a garbege collection (2 by default)";
  "-qgf", Arg.Set_int quick_gc_fuel, "set the number of times the scheduler can continuasely execute a pi_thread without a garbege collection after an unsuccessful garbege collection (2 by default)";
  "-af", Arg.Set_int active_factor, "set the ratio between the total waiting threads and the active waiting threads that when exceded involves a garbege collection (2 by default)";
  "-debug", Arg.Set debug, "put debugging trace in the generated code"
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
let nb_thread = !nb_thread
let std_gc_fuel = !std_gc_fuel
let quick_gc_fuel = !quick_gc_fuel
let active_factor = !active_factor
let debug = !debug
