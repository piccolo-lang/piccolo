
open Format
open SeqAST

let rec print_list f sep fmt = function
  | [] -> ()
  | [x] -> f fmt x
  | h :: t -> Format.fprintf fmt "%a%s@ %a" f h sep (print_list f sep) t

let rec print_list2 f1 f2 sep fmt l = match l with
  | [],[] -> ()
  | [], _ | _ , [] -> raise (Invalid_argument "List of different sizes")
  | [x],[y] ->fprintf fmt "%a %a" f1 x f2 y
  | (h1 :: t1, h2 :: t2) -> 
    fprintf fmt "%a %a%s@ %a" f1 h1 f2 h2 sep (print_list2 f1 f2 sep) (t1,t2)

let rec print_list_eol f sep fmt = function
  | [] -> ()
  | [x] -> fprintf fmt "%a%s" f x sep
  | h :: t -> fprintf fmt "%a%s@\n%a" f h sep (print_list_eol f sep) t

let rec print_list_eol' f sep fmt = function
  | [] -> ()
  | h :: t -> fprintf fmt "%a%s@\n%a" f h sep (print_list_eol' f sep) t


let print_string fmt str=
  fprintf fmt "%s" str

let rec print_piccType fmt = function 
  | Sty s -> fprintf fmt "%s" s
  | Pty (s,t) -> fprintf fmt "%s_%a" s print_piccType t
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
  | RecordName ((v,_),n) -> fprintf fmt "%a.%s" print_varName v n
  | ArrayName (v,e) -> fprintf fmt "%a[%a]" print_varName v print_expr e

and print_expr fmt = function
  | Val (v, _) -> fprintf fmt "%s" v
  | Var (v, _) -> fprintf fmt "%a" print_varName v
  | Op (op, e1, e2) -> fprintf fmt "%a %a %a" print_expr e1 print_binop op print_expr e2
  | CallFun ((f,_), args) -> 
    fprintf fmt "%a(@[ %a @])" print_varName f
      (print_list print_expr ", ") args

let rec print_instr fmt = function
  | Switch (e, il) -> 
    fprintf fmt "switch(%a){@[%a@]}" 
      print_expr e (print_list_eol' print_instr "") il
  | Case e -> fprintf fmt "case %a:" print_expr e
  | Bloc il -> fprintf fmt "{@[%a@]}" (print_list_eol' print_instr "") il
  | Seq il -> fprintf fmt  "%a" (print_list_eol' print_instr "") il
  | CallProc ((f,_), el) -> fprintf fmt "%a(@[ %a @]);" print_varName f
    (print_list print_expr ", ") el

  | Declare (v,t) -> fprintf fmt "%a %a;" print_piccType t print_varName v
  (*special case with array !*)
  | Assign ((v,_), e) -> fprintf fmt "%a = %a;" print_varName v print_expr e
  | DeclareFun (( v, (Fun (ret, argType))), args_names, il) ->
    fprintf fmt "%a %a(%a){@[%a@]}"
      print_piccType ret
      print_varName v
      (print_list2 print_string print_piccType ", ") (args_names, argType)
      (print_list_eol' print_instr "") il

  | DeclareFun (_,_,_) -> failwith "Must be Fun to be declared as function"
    
  | Foreach ((v,t), e, il) -> (* !!! *)
    fprintf fmt "foreach %a : %a in %a {@[%a@]}"
      print_varName v
      print_piccType t
      print_expr e
      (print_list_eol' print_instr "") il
  (* foreach (name : type) in (Fun) do () *)
  | Ite (e, il1, il2) ->
    fprintf fmt "if ( %a ){@[%a@]}else{@[%a@]}"
      print_expr e
      (print_list_eol' print_instr "") il1
      (print_list_eol' print_instr "") il2
      
  | Label s ->fprintf fmt "%s:" s
  | Goto s -> fprintf fmt "goto %s;" s
  | Return e -> fprintf fmt "return %a;" print_expr e
  | DoWhile (il, e) -> fprintf fmt "do{@\n%a}while(%a);" 
    (print_list_eol' print_instr "") il
    print_expr e
      
let print_instr_list_std il =
  List.iter (fun i -> Format.printf "%a@\n" print_instr i) il
