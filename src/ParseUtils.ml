
(* 
 module ParseUtils

   parser utilities *)

let parseFromString m = 
  let lexbuf = Lexing.from_string m
  in
  PilParser.moduleDef PilLexer.token lexbuf

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

