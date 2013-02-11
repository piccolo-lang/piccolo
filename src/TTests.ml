
(* module TTests 
   -------------

  Tests of Typing module

*)

(** Various tests for the Typing module *)

open Types;;  
open TypeRepr;; 
open Syntax;; 
open ASTRepr;;

let env_printer def = 
  let def = definition_type_of_definition def in
    Printf.printf "%s env :[ %s ]\n " def#name (String.concat ", " def#env )
;;

(* OK

let ppstr = "def ErrPingPong(i:chan<string>,o:chan<string>,i2:chan<int>,msg:string) = i2?(m), o!m, PingPong(i,o,msg)";;

let pp = ParseUtils.parseFromString ("module Test/PingPong \n" ^ ppstr ^ "\n def Main() = new(c1:chan<string>),new(c2:chan<string>),spawn{PingPong(c1,c2,\"<PING>\")}, end") ;;*)



let ppstr = "def ErrPingPong(i2:chan<int>,msg:string) = i2?(msg), ErrPingPong(i2,msg)";;

let pp = ParseUtils.parseFromString ("module Test/PingPong \n" ^ ppstr ^ "\n def Main() = new(c1:chan<int>),spawn{ErrPingPong(c1,\"<PING>\")}, end") ;;


let check_pp () = Middleend.first_pass pp 2 ;;

check_pp ();; 



(*
let check_pp = checkAndInferTypes pp ;;

let ppstr1 = "def PingPong(i:chan<string>,o:chan<string>,msg:string) = i?(m), o!msg, PingPong(i,o,msg)";;
let ppstr = "def ErrPingPong(i:chan<string>,o:chan<string>,i2:chan<int>,msg:string) = i2?(m), o!m, PingPong(i,o,msg)";;

env_printer (List.hd (module_type_of_module pp)#definitions);;

print_endline (string_of_module pp) ;;
*)
