
open Format
open PrintUtils
open SeqAST

let rec print_piccType fmt = function 
  | Sty s -> fprintf fmt "%s" s
  | Pty ("*",t) -> fprintf fmt "%a*" print_piccType t
  | Pty (s,t) -> fprintf fmt "%s%a" s print_piccType t
  | Fun (t, tl) -> 
    fprintf fmt "%a (@[ %a @])" print_piccType t 
      (print_list print_piccType ", ") tl
      
let print_binop fmt = function
  | Sum -> fprintf fmt "+"
  | Minus -> fprintf fmt "-"
  | Mult -> fprintf fmt "*"
  | Div -> fprintf fmt "/"
  | Equal -> fprintf fmt "=="

let rec print_varName fmt = function
  | SimpleName n -> fprintf fmt "%s" n
  | RecordName ((v, (Pty (nt, _))),n) -> 
    fprintf fmt (if nt = "*" then
	"%a->%s" 
      else
	"%a.%s") print_varName v n

  | RecordName ((v, _),n) -> fprintf fmt "%a.%s" print_varName v n
  | ArrayName (v,e) -> fprintf fmt "%a[%a]" print_varName v print_expr e

and print_expr fmt = function
  | Val (v, _) -> fprintf fmt "%s" v
  | Var (v, _) -> fprintf fmt "%a" print_varName v
  | Op (op, e1, e2) -> fprintf fmt "%a %a %a" print_expr e1 print_binop op print_expr e2
  | CallFun ((f,_), args) -> 
    fprintf fmt "%a(@[ %a @])" print_varName f
      (print_list print_expr ", ") args

let rec print_instr fmt = function
  | Comment str ->
      fprintf fmt "/* %s */@\n" str
  | Switch (e, il) -> 
    fprintf fmt "switch(%a){@\n@[<hov 3>%a@]@\n}" 
      print_expr e (print_list_eol print_instr "") il
  | Case e -> fprintf fmt "case %a:@\n" print_expr e
  | Bloc il -> fprintf fmt "{@\n@[%a@]@\n}" (print_list_eol print_instr "") il
  | Seq il -> fprintf fmt  "%a" (print_list_eol print_instr "") il
  | CallProc ((f,_), el) -> fprintf fmt "%a(@[ %a @]);" print_varName f
    (print_list print_expr ", ") el

  | Declare (v,t) -> fprintf fmt "%a %a;" print_piccType t print_varName v
  | Assign ((v,_), e) -> fprintf fmt "%a = %a;" print_varName v print_expr e
  
  | DeclareFun (( v, (Fun (ret, argType))), args_names, il) ->
    fprintf fmt "%a %a(%a){@\n@[<hov 3>%a@]@\n}"
      print_piccType ret
      print_varName v
      (print_list2 print_piccType print_string ", ") (argType, args_names)
      (print_list print_instr "") il

  | DeclareFun (_,_,_) -> failwith "Must be Fun to be declared as function"
    
  (* foreach (name : type) in (Fun) do () *)
  (* | Foreach ((v,t), e, il) -> (\* !!! *\) *)
  (*   fprintf fmt "foreach %a : %a in %a {@[%a@]}" *)
  (*     print_varName v *)
  (*     print_piccType t *)
  (*     print_expr e *)
  (*     (print_list_eol' print_instr "") il *)

  | Foreach ((v,_), e, il) -> (* !!! *)
    fprintf fmt 
      "PICC_KNOWNSET_FOREACH(PICC_Channel, %a, @ %a, it);@\n @[%a@] @\n END_KNOWNSET_FOREACH;"
      print_varName v
      print_expr e
      (print_list_eol' print_instr "") il

  | Ite (e, [], []) -> fprintf fmt ""
      
  | Ite (e, il1, []) ->
    fprintf fmt "if ( %a ){@\n@[<hov 3>%a@]}"
      print_expr e
      (print_list_eol' print_instr "") il1
  | Ite (e, il1, il2) ->
    fprintf fmt "if ( %a ){@\n@[<hov 3>%a@]}else{@\n@[<hov 3>%a@]}"
      print_expr e
      (print_list_eol' print_instr "") il1
      (print_list_eol' print_instr "") il2
  
  | Label s ->fprintf fmt "%s:" s
  | Goto s -> fprintf fmt "goto %s;" s
  | Return e -> fprintf fmt "return %a;" print_expr e
  | DoWhile (il, e) -> fprintf fmt "do{@\n%a}while(%a);" 
    (print_list_eol' print_instr "") il
    print_expr e
      

let print_main nb_th entry_point eSize kSize enabled fmt i =
  let inc_list = 
    ["#include <runtime.h>";
     "#include <value.h>";
     "#include <queue.h>";
     "#include <pi_thread_repr.h>";
     "#include <commit_repr.h>";
     "#include <scheduler_repr.h>"
     ]
  in
  Format.fprintf fmt
    "%a@\n@\n@\n@\n%a@\n@\n@\n@\nint main(){ PICC_main(%d, %s, %d, %d, %d); return 0;}"
    PrintUtils.(print_list_eol print_string "") inc_list
    print_instr i nb_th entry_point eSize kSize enabled
    

let print_instr_list_std il =
  set_margin 150;
  List.iter (fun i -> Format.printf "%a@\n" print_instr i) il
