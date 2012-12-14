
(* type label =  *)
(* | DefLabel of int *)
(* | ContLabel of string *)


type 
  varName =
  | SimpleName of string            (* name *)
  | RecordName of varName * varName (* name.subField *)
  | ArrayName  of varName * int     (* name[i] or name.subField[i] *)
and
  piccType =
  | Void
  | PBool
  | PInt
  | PValue
  | SchedPool
  | PiThread
  | Channel

  | Queue

  | Lock

  | Commit 
  | InCommit
  | OutCommit
  
  | DLabel (* Definition label *)
  | Fun of piccType * ( piccType list )
  | KnowsSet (* of piccType *)
  | PSet of piccType
  | TryResultEnum
  | StatusEnum
and 
  (* [TODO]? utiliser des ref pour que si on modifie un var toutes ses occurences soient modifiés en même temps?? *)
  varDescr = varName * piccType


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


type instr =
  | Bloc of instr list (* real bloc semantic *)
  | Seq of instr list (* just an instr list handler *)
  | Call of varDescr * (varDescr list) (* piccType = Fun *)
  | Declaration of varDescr
  | Assignment of varDescr * expr
  | Foreach of varDescr * varDescr * instr (* foreach (name : type) in (Fun) do () *)
  | Ite of expr * instr * instr
  | GLabel of string (*Goto Label *)
  | Goto of string
