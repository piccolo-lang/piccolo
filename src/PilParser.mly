/* header */
%{
  (** Parser for Pi-Thread *)
 
  open Printf ;;
  open Lexing ;;

  open Utils ;;

  open Types ;;
  open TypeRepr ;;
  open Syntax ;;
  open ASTRepr ;;

  let current_module = ref "" ;;
  let current_definition = ref "" ;;

  exception Fatal_Parse_Error of string ;;



  let fatal_parse_error (msg:string) (start_pos:position) (end_pos:position) =
    printf "Fatal parse error at line %d (character %d)\n  ==> %s\n" (start_pos.pos_lnum) (start_pos.pos_cnum - start_pos.pos_bol) msg ;
    raise (Fatal_Parse_Error msg) ;;

%}

/* reserved keywords */
%token MODULE DEF VTRUE VFALSE END NEW SPAWN TAU LET

/* identifiers */
%token <string> IDENT

/* constants */
%token <int> INT
%token <bool> TRUE
%token <bool> FALSE
%token <string> STRING

/* punctuation */
%token LPAREN RPAREN LBRACKET RBRACKET LCURLY RCURLY INF SUP SLASH SHARP STAR EQ COLON

/* operators */
%token PLUS COMMA OUT IN

%left PLUS
%right COMMA
%left OUT IN

/* end of file */

%token EOF

  /* types */

%token TBOOL TINT TSTRING TCHAN

%start moduleDef
%type <Syntax.moduleDef> moduleDef
%type <string> moduleID
%type <definition> definition
%type <process> process
%type <action> action
%type <value*valueType> value
%type <(value*valueType) list> values

  /* grammar */
%%
moduleDef: moduleDeclaration definitions EOF { makeModule $1 $2 }

moduleDeclaration :
| MODULE moduleID { current_module := $2; $2 }
| MODULE error { fatal_parse_error "Wrong module identifier" (Parsing.rhs_start_pos 1) (Parsing.rhs_end_pos 1) }
| error { fatal_parse_error "Missing module declaration" (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos()) }

moduleID: 
| IDENT { $1 }
| IDENT SLASH moduleID { $1 ^ $3 }

definitions: 
| definition { [$1] }
| definition definitions { $1::$2 }

definition: DEF IDENT paramlist EQ process { makeDefinition $2 $3 $5 }

paramlist: 
| LPAREN RPAREN { [] }
| LPAREN params RPAREN { $2 }

params: 
| param { [$1] }
| param COMMA params { $1::$3 }

param: 
| IDENT COLON typeDef { ($1, $3) }
| IDENT { ($1, TUnknown) }
| error { raise (Fatal_Parse_Error("Wrong parameter")) }

/* processes */

process:
| END { makeTerm !current_module !current_definition }
| call { $1 }
| choiceProcess { makeChoice !current_module !current_definition $1 }

call:
| moduleID COLON IDENT LPAREN RPAREN { makeCall !current_module !current_definition $1 $3 [] [] }
| moduleID COLON IDENT LPAREN values RPAREN { makeCall !current_module !current_definition $1 $3 (List.map snd $5) (List.map fst $5) }
| IDENT LPAREN RPAREN { makeCall !current_module !current_definition !current_module $1 [] [] }
| IDENT LPAREN values RPAREN { makeCall !current_module !current_definition !current_module $1 (List.map snd $3) (List.map fst $3) }

choiceProcess:
| branch { [$1] }
| branch PLUS choiceProcess { $1::$3 }

branch:
| LBRACKET value RBRACKET action COMMA process { (fst $2, snd $2, $4, $6) }
| action COMMA process { (makeVTrue (), TBool, $1, $3) }

action: 
| TAU { makeTau () }
| IDENT OUT value { makeOutput $1 (fst $3) (snd $3) }
| IDENT IN LPAREN IDENT RPAREN { makeInput $1 $4 TUnknown }
| NEW LPAREN IDENT COLON typeDef RPAREN { makeNew $3 $5 }
| SPAWN LCURLY call RCURLY { makeSpawnCall $3 }
| SPAWN LCURLY error {  fatal_parse_error "Expecting call" (Parsing.rhs_start_pos 3) (Parsing.rhs_end_pos 3) }
| SHARP moduleID COLON IDENT LPAREN RPAREN { makePrim $2 $4 [] [] }
| SHARP moduleID COLON IDENT LPAREN values RPAREN { makePrim $2 $4 (List.map snd $6) (List.map fst $6) }
| LET LPAREN IDENT COLON typeDef EQ value RPAREN { makeLet $3 $5 (fst $7) (snd $7) }

/* types */

typeAtomic: 
| TBOOL { TBool }
| TINT { TInt }
| TSTRING { TString }
| error { raise (Fatal_Parse_Error("Unknown atomic type")) }

typeDef: 
| TCHAN INF typeDef SUP { TChan $3 }
| LPAREN types RPAREN { makeTupleType $2 }
| typeAtomic { $1 }
| error { raise (Fatal_Parse_Error("Wrong type")) }

types : 
| typeDef { [$1] }
| typeDef STAR types { $1::$3 }

/* values */

values : 
| value { [$1] }
| value COMMA values { $1::$3 }

value : 
| VTRUE { (makeVTrue (), TBool) }
| VFALSE { (makeVFalse (), TBool) }
| INT { (makeVInt $1, TInt) }
| STRING { (makeVString (String.sub $1 1 ((String.length $1) - 2)), TString) }
| LPAREN values RPAREN { let ts = List.map snd $2 in (makeTuple ts (List.map fst $2), makeTupleType ts)}
| IDENT { (makeVVar TUnknown $1, TUnknown) }
| SHARP moduleID COLON IDENT LPAREN RPAREN { (makeVPrim $2 $4 [] TUnknown [], TUnknown) }
| SHARP moduleID COLON IDENT LPAREN values RPAREN { (makeVPrim $2 $4 (List.map snd $6) TUnknown (List.map fst $6), TUnknown) }
| error { fatal_parse_error "Wrong value" (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos()) }

%%
