(* 
   module ParseUtils
   --------------------

   Parser utilities *)
(** This module defines various utility functions used in the Parser module. *)

(** exception for parse error*)
exception Report_parse_error of string

(** print the line and characters of a parsing error *) 
let report_loc lb =
  Lexing.(
    let b = lexeme_start_p lb in
    let e = lexeme_end_p lb in
    let l = b.pos_lnum in
    let fc = b.pos_cnum - b.pos_bol + 1 in
    let lc = e.pos_cnum - b.pos_bol + 1 in
      Printf.sprintf "Syntax error line %d, characters %d-%d" l fc lc)

(** parse a string into a module *)    
let parseFromString s = 
  let lexbuf = Lexing.from_string s in
    try 
      PilParser.moduleDef PilLexer.token lexbuf
    with Parsing.Parse_error -> raise (Report_parse_error (report_loc lexbuf))
      
(** parse a string into a definition *)
let parseDefinitionFromString def =
  let m = parseFromString ("module test\n" ^ def) in
    match m with
	Syntax.Module m' ->
	  match m'#definitions with
	      d::_ -> d
	    | [] -> assert false
		
(** parse a string into a process *)
let parseProcessFromString proc =
  let def = parseDefinitionFromString ("def Test() = " ^ proc) in
    match def with
	Syntax.Def d' -> d'#process
	  
(** parse the content of a file into a module *) 
let parseFromFile fname =
  let ch = open_in fname in
  let lexbuf = Lexing.from_channel ch in
    PilParser.moduleDef PilLexer.token lexbuf

