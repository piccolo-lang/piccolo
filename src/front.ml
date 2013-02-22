
module CBackend = Backend.Make (SeqASTConstC) (SeqASTPrettyPrinterC)

let _ =
  let mod_def = ParseUtils.parseFromFile Settings.filename in
  (* let (_,errors) = Middleend.first_pass mod_def Settings.verbose in *)
  let entry_point, c_code = CBackend.compile_module mod_def in
  let formatter = Format.formatter_of_out_channel (open_out Settings.outname) in
  Format.fprintf formatter "%a@\n" 
    CBackend.(print_main Settings.nb_thread entry_point) c_code
      
