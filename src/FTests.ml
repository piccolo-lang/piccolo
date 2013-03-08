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
  Printf.printf "\n%s env :[ %s ], esize : %d , csize : %d , nbchanels : %d , nbchoice : %d  " def#name (String.concat ", " def#env ) def#esize def#csize def#nbChannels def#nbChoiceMax

let ppstr2 = "def PingPong2(o:chan<string>,i:chan<string>,msg:string) = o!msg, new(a:chan<string>), i?(m), PingPong(i,o,m)";;

let ppstr = "def PingPong(i:chan<string>,o:chan<string>,msg:string) =  i?(m), new(z:chan<string>), o!msg, PingPong2(i,o,m) + o!msg, new(z:chan<string>), PingPong2(i,o,m)" ;;

let mainstr = "def Main() = new(c1:chan<string>),new(c2:chan<string>),spawn{PingPong(c1,c2,\"<PING>\")},spawn{PingPong2(c1,c2,\"<PONG>\")},end";;

let ppstr3 = "def PingPong2(o:chan<string>,i:chan<string>,msg:string) = 
[true] o!msg, i?(m), PingPong2(i,o,m) + 
[true] i?(m), o!msg, o!msg, PingPong(i,o,m)";;


let pp = ParseUtils.parseFromString ("module Test/PingPong \n" ^ ppstr ^ "\n" ^ ppstr2 ^ "\n" ^ mainstr) ;;

let check_pp () = Middleend.compute_pass pp 1;;

check_pp ();;

Printf.printf "\n\n";;
(List.iter (fun def -> env_printer def) (module_type_of_module pp)#definitions);;
Printf.printf "\n\n";;
print_endline (string_of_module pp) ;;
