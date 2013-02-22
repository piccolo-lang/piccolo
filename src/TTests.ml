
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

let test_module = "def End() = end";; (* ok *)

let test_call1 = "def Call1() = Test/Test1:test()";; (* passage call *)

let test_call2 = "def Call2() = Test/Test1:test(true)";; (* ok *)

<<<<<<< HEAD
let test_call3 = "def Call3() = test()";; (* passage call *)

let test_call4 = "def Call4() = test(false)";; (* ok *)

let test_choice1 = "def ActionTau() = tau, end";; (* ok *)
=======
(* let ppstr1 = "def PingPong(i:chan<string>,o:chan<string>,msg:string) = i?(m), o!msg, PingPong(i,o,msg )";; *)
(* let ppstr = "def ErrPingPong(i:chan<string>,o:chan<string>,i2:chan<int>,msg:string) = i2?(m), o!m, PingPong(i,o,msg)";; *)

let ppstr = "def PingPong(i:chan<string>,o:chan<string>,msg:string) = i?(m), #core/io:println(m), o!msg, PingPong(i,o,msg)";;
let ppstr = "def ErrPingPong(i2:chan<string>,msg:string) = i2?(msg), ErrPingPong(i2,msg)";;
>>>>>>> 1d510173eb4858f55676c637d48258a46350a2db

let test_choice2 = "def Out() = c!toto, end";; (* non tuple *)

let test_choice3 = "def In() = in?(42), end";; (* parse error *)

let test_branch1 = "def Branch() = [42]tau, end";; (* ok *)

let test = ParseUtils.parseFromString("module Test/Test1 \n" ^ test_choice2 );;

let check_pp () = Middleend.first_pass test 5;;

check_pp ();; 



