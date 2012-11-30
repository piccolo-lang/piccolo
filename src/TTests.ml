
(* module TTests 
   -------------

  Tests of Typing module

*)

(** Various tests for the Typing module *)

open Types ;;  
open TypeRepr ;; 
open Syntax ;; 
open ASTRepr ;;

let ppstr = "def PingPong(i:chan<string>,o:chan<string>,msg:string) = i?(m), #core/io:println(m), o!msg, PingPong(i,o,msg)";;

let pp = ParseUtils.parseFromString ("module Test/PingPong \n" ^ ppstr ^ "\n def Main() = new(c1:chan<string>),new(c2:chan<string>),spawn{PingPong(c1,c2,\"<PING>\")},spawn{PingPong(c2,c1,\"<PONG>\")},c1!\"<INIT>\",end") ;;

let check_pp () = Middleend.first_pass pp 3 ;;

check_pp () ;;

(*
let check_pp = checkAndInferTypes pp ;;

*)
