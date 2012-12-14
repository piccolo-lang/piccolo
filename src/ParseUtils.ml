
(** This module defines various utility functions used in the Parser module. *)

(* 
 module ParseUtils

   parser utilities *)

exception Report_parse_error of string

let report_loc lb =
  Lexing.(
    let b = lexeme_start_p lb in
    let e = lexeme_end_p lb in
    let l = b.pos_lnum in
    let fc = b.pos_cnum - b.pos_bol + 1 in
    let lc = e.pos_cnum - b.pos_bol + 1 in
      Printf.sprintf "Syntax error line %d, characters %d-%d" l fc lc)

let parseFromString m = 
  let lexbuf = Lexing.from_string m
  in
    try 
      PilParser.moduleDef PilLexer.token lexbuf
    with Parsing.Parse_error -> raise (Report_parse_error ( report_loc lexbuf ))
      

let parseDefinitionFromString def =
  let m = parseFromString ("module test\n" ^ def)
  in match m with Syntax.Module m' -> match m'#definitions with d::_ -> d | [] -> assert false

let parseProcessFromString proc =
  let def = parseDefinitionFromString ("def Test() = " ^ proc)
  in match def with Syntax.Def d' -> d'#process

let parseFromFile fname =
  let ch = open_in fname
  in let lexbuf = Lexing.from_channel ch
     in PilParser.moduleDef PilLexer.token lexbuf

