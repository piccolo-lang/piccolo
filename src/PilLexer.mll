{
  (** Lexer for Pi-Thread *)
  open PilParser ;;

  let incr_linenum lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { pos with
      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
      Lexing.pos_bol = pos.Lexing.pos_cnum;
    }
  ;;
}

let eol = '\n'
let ident = ['a'-'z''A'-'Z''_'] ['a'-'z''A'-'Z''0'-'9''_']*('\'')*
let digit = ['0'-'9']
let int = (['1'-'9'] digit*)

let line_cmt = ("--"[^'\n']*)

let r_module = "module"
let r_def = "def"
let r_true = "true"
let r_false = "false"
let r_end = "end"
let r_new = "new"
let r_tau = ("skip" | "tau")
let r_spawn = "spawn"
let r_let = "let"

let r_bool = "bool"
let r_int = "int"
let r_chan = "chan"
let r_string = "string"

let comma = ','
let qmark = '\"'
let slash = '/'
let eq = '='
let colon = ':'

let str = (qmark ([^'\"']|'\\''\"')* qmark)

let op_plus = '+'
let op_out = '!'
let op_in = '?'
let op_sharp = '#'
let op_star = "*"

let inf = '<'
let sup = '>'
let lparen = '('
let rparen = ')'
let lbracket = '['
let rbracket = ']'
let lcurly = '{'
let rcurly = '}'

let sep = (['\t' ' ']+)

rule token = parse
  | sep { token lexbuf }
  | eol { Lexing.new_line lexbuf;
          token lexbuf }
  | line_cmt { Lexing.new_line lexbuf;
               token lexbuf }
  | digit as n { INT(int_of_string (Char.escaped n)) }
  | int as n { INT(int_of_string n) }
  | str as s { STRING(s) }
  | r_module { MODULE }
  | r_def { DEF }
  | r_true { VTRUE }
  | r_false { VFALSE }
  | r_end { END }
  | r_tau { TAU }
  | r_new { NEW }
  | r_spawn { SPAWN }
  | r_let { LET }
  | r_bool { TBOOL }
  | r_int { TINT }
  | r_chan { TCHAN }
  | r_string { TSTRING }
  | inf { INF }
  | sup { SUP }
  | lparen { LPAREN }
  | rparen { RPAREN }
  | lbracket { LBRACKET }
  | rbracket { RBRACKET }
  | lcurly { LCURLY }
  | rcurly { RCURLY }
  | comma { COMMA }
  | slash { SLASH }
  | eq { EQ }
  | colon { COLON }
  | op_plus { PLUS }
  | op_out { OUT }
  | op_in { IN }
  | op_sharp { SHARP }
  | op_star { STAR }
  | ident as id { IDENT (id) }
  | eof { EOF }
  | _ { failwith((Lexing.lexeme lexbuf) ^ " : syntax error") }
        (* ^ ": mistake at line " ^ (string_of_int (Lexing.lexeme_start_p lexbuf).pos_lnum)  
    ^ " column " ^ (string_of_int ((Lexing.lexeme_start_p lexbuf).pos_cnum)))} *)

{
}
