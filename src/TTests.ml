(* module TTests 
   -------------

  Tests of Typing module

*)
(** Various tests for the Typing module. *)

open Types;;  
open TypeRepr;; 
open Syntax;; 
open ASTRepr;;

let test_end = "def End() = end";; (* ok *)

let test_call1 = "def Call1() = Test/Test1:Call1()";; (* ok *)

let test_call2 = "def Call2() = Test/Test1:Call2(true)";; (* arity problem *)

let test_call3 = "def Call3() = test()";; (* definition not found problem handled *)

let test_call4 = "def Call4(x:bool) = Call4(false)";; (* ok *)

let test_call5 = "def Call5(x:int) = Call5(false)";; (* ok *)

let test_param1 = "def Param1(toto) = end";; (* ok *)

let test_param2 = "def Param2(id1:bool) = end";; (* ok *)

let test_param3 = "def Param3(id1:int, id2:string, id3:chan<bool>) = end";; (* ok *) 

let test_choice1 = "def ActionTau() = tau, end + tau, end";; (* ok *)

let test_choice2 = "def Out(c:chan<int>) = c!(42, true, false), end";; (* missmatch ok *)

let test_choice3 = "def In(i:chan<int>) = i?(toto), end";; (* ident no toto ok *)

let test_tuple1 = "def Out(c:chan<(int*bool*bool)>) = c!(42, true, false), end";; (* ok *)

let test_tuple2 = "def Out(c:chan<int>) = c!(42, (true, false), 44), end";; (* tuple in tuple type error, ok *)

let test_tuple3 = "def Out(c:chan<(int * int)>) = c!(42, #core/arith:add(3, 4)), end";;

let test_tuple4 = "def Tuple4(i:int) = let(x:(bool*int*(bool*string)*int)=(true,#core/arith:add(5, 6),(true, \"toto\"), i)), end";;

let test_branch1 = "def Branch1() = [false]tau, end";; (* ok *)

let test_branch2 = "def Branch2(c:chan<string>, i:chan<string>, toto:string) = i?(toto), end + c!toto, end";; (* ok *)

let test_branch3 = "def Branch3(c:chan<string>, i:chan<int>) = i?(toto), end + c!\"toto\", end";; (* setting chan ok *)

let test_branch4 = "def Branch4(c:chan<(int * string)>) = tau, end";; (* ok *)

let test_branch5 = "def Branch5() = [42]tau, end";; (* ok *)

let test_new1 = "def New1() = new(id1:bool), end";; (* OK? *)

let test_new2 = "def New2() = new(c:chan<string>), end";; (* OK? *)

let test_new3 = "def New3() = new(c:chan<string>), New3(c1)";; (* NON *)

let test_new4 = "def New4() = new(c:chan<int>), end";; (* OK? *)

let test_new5 = "def New5(c:chan<int>) = new(c:chan<bool>), end";;

let test_let1 = "def Let1() = let(id1:int=42), end";; (* ok *)

let test_let2 = "def Let2() = let(t:(int*bool*int)=(42,true,44)), end";; (* ok *)

let test_let3 = "def Let3() = let(t:chan<int>=true), end";; (* to ask ! ! ! *)

let test_let4 = "def Let4(x:int) = let(x:bool=true), end";; (* ok *)

let test_let5 = "def Let5(x:int) = let(x:int=y), end";; (* DANGER *)

let test_let6 = "def Let6(y:string) = let(x:int=y), end";; (* ok *)

let test_let7 = "def Let7(i:chan<bool>, x:string) = let(x:bool=true), i?(x), end";; (* ok *)

let test_let8 = "def Let8(i:chan<bool>, x:string) = let(x:int=42), i?(x), end";; (* ok *)

let test_let9 = "def Let9(i:chan<string>) = let(x:bool=true), i?(x), end";; (* ok *)

let test_out1 = "def Out1(c:chan<bool>) = c!true, end";; (* ok *)

let test_out2 = "def Out2(c:chan<bool>, d:chan<int>) = c!false, d!42, end";; (* ok *)

let test_out3 = "def Out3(c:chan<string>) = c!\"toto\", Test1:Out3()";; (* arity *)

let test_out4 = "def Out4(c:chan<(int * bool)>) = c!(42, false), Out4(\"toto\")";; (* arity *)

let test_out5 = "def Out5(c:chan<string>, toto:string) = c!toto, end";; (* GO BACK HERE *)

let test_out6 = "def Out6(c:chan<string>, toto:string) = c!toto, Out6(toto)";; (* ok arity *)

let test_out7 = "def Out7(c:chan<int>, toto:int, titi:string) = c!toto, Out7(c, 42, titi)";; (* probleme out *)

let test_in1 = "def In1(i:chan<string>, toto:string) = i?(toto), end";; (* ok *)

let test_in2 = "def In2(i:chan<chan<bool>>, toto:string) = i?(toto), end";; (* in setting, ok *)

let test_in3 = "def In3(i:chan<string>, toto:string) = i?(toto), In3(toto)";; (* arity *)

let test_in4 = "def In4(toto:int) = In4(toto)";; (* ok *)

let test_in5 = "def In5(i:chan<string>, toto:int) = i?(toto), In5(i, toto)";; (* ok, finally ! *)

let test_in6 = "def In6(i:chan<int>) = i?(toto), end";; (* ok *)

let test_in34 = "def In3(i:chan<string>, toto:string) = i?(toto), In4(i, toto) \n def In4(i:chan<string>, toto:string) = i?(toto), In4(i, toto)";; (* ok *)

let test_in43 = "def In43(i:chan<string>, toto:string) = i?(toto), In443(i, toto) \n def In443(i:chan<string>, toto:int) = i?(toto), In443(i, toto)";; (* ok *)

let test_prim_action1 = "def PrA1() = #core/io:print_int(42), end";; (* ok *)

let test_prim_action2 = "def PrA2() = #core/io:print_str(\"toto\", 42), end";; (* arity in prim ok *)

let test_prim_action3 = "def PrA3() = #core/io:print_str(true), end";; (* arg type check ok *)

let test_prim_action4 = "def PrA4(m:string) = #core/io:print_str(m), end";; (* ok *)

let test_prim_action5 = "def PrA5(m:string, n:int) = #core/io:print_str(m, true, n, false), end";; (* setting var type ok *)

let test_prim_action6 = "def PrA6() = #core/io:fake_prim(42), end";; (* not found prim ok *)

let test_prim_action7 = "def PrA7() = let(x:int=42), #core/io:print_int(x), end";; (* ok *)

let test_prim_action8 = "def PrA8() = let(x:bool=true), #core/io:print_int(x), end";; (* expected arg type ok *)

let test_prim_action9 = "def PrA9(i2:chan<int>) = i2?(m), #core/io:print_str(m), end";; (* ok *)

let test_prim_value1 = "def PrV1() = let(x:string=#core/arith:add(true, 2)), end";; (* NON *)

let test_prim_value2 = "def PrV2(c:chan<int>) = c!#core/arith:substract(2, 3), end";; (* ok for chanel *)

let test_prim_value3 = "def Prv3() = [#core/arith:compare(2, 3)]tau, end";; (* ok for guard *)

let test_prim_value4 = "def PrV4(a:int) = let(x:int=#core/arith:add(a, 2)), end";; (* ok for var *)

let test_prim_value5 = "def PrV5() = let(x:int=#core/arith:add(a, 2)), end";; (* ok not found *)

let test_prim_value6 = "def PrV6(a:int, b:string) = let(x:int=#core/arith:add(a, b)), end";; (* ok var *)

let test_prim_value7 = "def PrV7() = let(x:int=#core/arith:add(1, 2, 3)), end";; (* ok arity *)

let test_prim_value8 = "def PrV8() = let(x:string=#core/arith:add(1, 2)), end";; (* ok *)

let test_prim_value9 = "def PrV9() = let(x:int=#core/arith:substract(#core/arith:add(1, #core/arith:substract(9, 8)), 42)), end";; (* ok *)

let test_spawn1 = "def PingPong(i:int,o:bool,msg:string) = tau, end \n " ^ "def Spawn1() = spawn{PingPong(42, true, \"toto\")}, end";;

let test_spawn2 = "def PingPong(i:int,o:bool,msg:string) = tau, end \n " ^ "def Spawn2() = spawn{PingPong(42, \"toto\")}, end";;

let test_spawn3 = "def PingPong(i:int,o:bool,msg:string) = tau, end \n " ^ "def Spawn3() = spawn{PingPong(42, \"toto\", 42)}, end";;

let test_spawn4 = "def PingPong(i:int,o:bool,msg:string) = tau, end \n " ^ "def Spawn4(v:int) = spawn{PingPong(v, true, \"toto\")}, end";;

let test_spawn5 = "def PingPong(i:int,o:bool,msg:string) = tau, end \n " ^ "def Spawn5() = spawn{Ping(42, true, \"toto\")}, end";; (* not found handled *)

let test_spawn6 = "def PingPong(i:chan<int>,o:bool,msg:string) = tau, end \n " ^ "def Spawn6(c:chan<int>) = spawn{PingPong(c, true, \"toto\")}, end";;

let test_spawn7 = "def PingPong(i:int,o:bool,msg:string) = tau, end \n " ^
  "def Spawn7() = spawn{PingPong(#core/arith:add(1, 2), true, \"toto\")}, #core/io:print_str(\"titi\"), end";;

let ppstr1 = "def ErrPingPong(i2:chan<string>,msg:string) = i2?(msg), ErrPingPong(i2,msg)";; (* ok *)

let ppstr2 = "def ErrPingPong(i2:chan<string>,msg:int) = i2?(msg), ErrPingPong(i2,msg)";; (* ok *)

let ppstr3 = "def PingPong(i:chan<string>,o:chan<string>,msg:string) = i?(m), #core/io:print_str(msg), o!msg, PingPong(i,o,msg)";; (* ok *)

let ppstr4 = "def PingPong(i:chan<string>,o:chan<string>,msg:string) = i?(m), o!msg, PingPong(i,o,msg)";; (* ok *)

let ppstr5 = "def ErrPingPong(i:chan<string>,o:chan<int>,i2:chan<int>,msg:bool) = i2?(m), o!m, ErrPingPong(i,o,i2,msg)";; (* ok *)

let fibStr = "def Fibonacci(n:int,m:int,p:int,r:chan<int>)=[#core/arith:compare(n,0)]r!m,end+tau,Fibonacci(#core/arith:substract(n,1),#core/arith:add(m, p),m,r)";;

let main = "def Main() = new(r:chan<int>), spawn{Fibonacci(3,4,5,r)},#core/io:print_str(\"toto\"),Fibonacci(3,4,5,r)";;

let testmain = ParseUtils.parseFromString ("module Test/Fibonacci \n" ^ fibStr ^ "\n" ^ main );;

let test = ParseUtils.parseFromString ("module Test/Test \n" ^ test_branch2);;

let check_pp () = Middleend.compute_pass test 5;;

check_pp ();; 






