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
    Printf.printf "%s env :[ %s ]\n " def#name (String.concat ", " def#env)
;;

let test_end = "def End() = end";; (* ok *)

let test_call1 = "def Call1() = Test/Test1:test()";; (* ok *)

let test_call2 = "def Call2() = Test/Test1:test(true)";; (* ok *)

let test_call3 = "def Call3() = test()";; (* ok *)

let test_call4 = "def Call4() = test(false)";; (* ok *)

let test_param1 = "def Param1(toto) = end";; (* enregistrement des params *)

let test_param2 = "def Param2(id1:bool) = end";; 

let test_param3 = "def Param3(id1:int, id2:string, id3:chan<bool>) = end";; 

let test_choice1 = "def ActionTau() = tau, end";; (* ok *)

let test_choice2 = "def Out(c:chan<int>) = c!(42, true, false), end";; (* ok *)

let test_choice3 = "def In(i:chan<int>) = i?(toto), end";; (* ident *)

let test_tuple1 = "def Out(c:chan<(int*bool*bool)>) = c!(42, true, false), end";; (* ok *)

let test_tuple2 = "def Out(c:chan<(int*(bool*bool)*bool)>) = c!(42, (true, false), false), end";; (* ?? *)

let test_branch1 = "def Branch1() = [\"toto\"]tau, end";; (* ok *)

let test_branch2 = "def Branch2(c:chan<string>, i:chan<string>) = i?(toto), end + c!\"toto\", end";;

let test_branch3 = "def Branch3(c:chan<(int * string)>) = tau, end";;

let test_action1 = "def Def1() = spawn{PingPong(true, false, true)}, spawn{PongPing((true, false, true))}, end";; (* multiple spawn in *)

let test_action2 = "def Def2() = new(id1:bool), end";;

let test_action3 = "def Def3() = let(id1:int=42), end";;

let test_out1 = "def Out1(c:chan<bool>) = c!true, end";; (* ok *)

let test_out2 = "def Out2(c:chan<bool>, d:chan<int>) = c!false, d!42, end";; (* ok *)

let test_out3 = "def Out3(c:chan<string>) = c!\"toto\", Test1:No()";; (* ok *)

let test_out4 = "def Out4(c:chan<(int * bool)>) = c!(42, false), No(\"toto\")";; (* ok *)

let test_out5 = "def Out5(c:chan<int>, toto:string) = c!toto, end";; (* ok *)

let test_out6 = "def Out6(c:chan<string>, toto:string) = c!toto, Out6(toto)";; (* ok *)

let test_out7 = "def Out7(c:chan<int>, toto:int, titi:string) = c!toto, Out7(titi)";; (* ok *)

let test_in1 = "def In1(i:chan<string>, toto:string) = i?(toto), end";;

let test_in2 = "def In2(i:chan<chan<bool>>, toto:string) = i?(toto), end";;

let test_in3 = "def In3(i:chan<string>, toto:string) = i?(toto), In3(toto)";;

let test_in4 = "def In4(toto:int) = In4(toto)";;

let test_in5 = "def In5(i:chan<string>, toto:int) = i?(toto), In5(i, toto)";;

let test_in34 = "def In3(i:chan<string>, toto:string) = i?(toto), In4(toto) \n def In4(i:chan<string>, toto:int) = i?(toto), In3(toto)";;

let test_in9 = "def In9(c:chan<int>, toto:int, titi:string) = i?(toto), In9(titi)";;

let ppstr1 = "def ErrPingPongr(i2:chan<string>,msg:string) = i2?(msg), ErrPingPong(i2,msg)";;

let ppstr2 = "def ErrPingPongr(i2:chan<string>,msg:int) = i2?(msg), ErrPingPong(i2,msg)";;

let test = ParseUtils.parseFromString("module Test/Test1 \n" ^ test_in5 );;

let check_pp () = Middleend.first_pass test 5;;

check_pp ();; 



(* let ppstr1 = "def PingPong(i:chan<string>,o:chan<string>,msg:string) = i?(m),
 o!msg, PingPong(i,o,msg )";; *)
(* let ppstr = "def ErrPingPong(i:chan<string>,o:chan<string>,i2:chan<int>,msg:s
tring) = i2?(m), o!m, PingPong(i,o,msg)";; *)

let ppstr = "def ErrPingPong(i2:chan<string>,msg:string) = i2?(msg), ErrPingPong
(i2,msg)";;

