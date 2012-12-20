
(* type label =  *)
(* | DefLabel of int *)
(* | ContLabel of string *)


type varName =
| SimpleName of string            (* name *)
| RecordName of varName * string (* name.subField *)
| ArrayName  of varName * int     (* name[i] or name.subField[i] *)

type piccType =
| Sty of string (* Simple Type *)
| Pty of string * piccType (* Parameterized type *)
| Fun of piccType * ( piccType list ) 

(* [TODO]? utiliser des ref pour que si on modifie une var,
   toutes ses occurences soient modifiés en même temps?? *)
type varDescr = varName * piccType

type binop = 
| Sum
| Minus
| Mult
| Div
| Equal

type value_t = string * piccType

type expr =
| Val of value_t
| Var of varDescr
| Op of binop * expr * expr
| CallFun of varDescr * (expr list)

type instr =
| Bloc of instr list (* real bloc semantic *)
| Seq of instr list (* just an instr list handler *)
| CallProc of varDescr * (expr list) (* Call procedure *)
| Declare of varDescr
| Assign of varDescr * expr
| DeclareFun of varDescr * (instr list)
| Foreach of varDescr * expr * (instr list) (* foreach (name : type) in (Fun) do () *)
| Ite of expr * (instr list) * (instr list)
| Label of string
| Goto of string
| Return of expr
