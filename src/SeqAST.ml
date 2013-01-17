
(* type label =  *)
(* | DefLabel of int *)
(* | ContLabel of string *)


type varName =
| SimpleName of string           (* name *)
| RecordName of varName * string (* name.subField *)
| ArrayName  of varName * expr    (* name[i] or name.subField[i] *)

and piccType =
| Sty of string (* Simple Type *)
| Pty of string * piccType (* Parameterized type *)
| Fun of piccType * ( piccType list ) 

and varDescr = varName * piccType
(* [TODO]? utiliser des ref pour que si on modifie une var,
   toutes ses occurences soient modifiés en même temps?? *)

and value_t = string * piccType

and expr =
| Val of value_t
| Var of varDescr
| Op of binop * expr * expr
| CallFun of varDescr * (expr list)

and binop = 
| Sum
| Minus
| Mult
| Div
| Equal

type instr =
| Switch of expr * (instr list)
| Case of expr
| Bloc of instr list (* real bloc semantic *)
| Seq of instr list (* just an instr list handler *)
| CallProc of varDescr * (expr list) (* Call procedure *)
| Declare of varDescr
| Assign of varDescr * expr
| DeclareFun of varDescr * (string list) *(instr list)
| Foreach of varDescr * expr * (instr list) (* foreach (name : type) in (Fun) do () *)
| Ite of expr * (instr list) * (instr list)
| Label of string
| Goto of string
| Return of expr
| DoWhile of (instr list) * expr
