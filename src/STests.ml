
(* module STests 
   -------------

  Tests of Syntax module

*)

open Types ;;
open TypeRepr ;;
open Syntax ;;
open ASTRepr ;;


(* value types *)

print_endline "----------------- VALUE TYPES -----------------" ;;

let t1 = TChan (makeTupleType [ TInt ; TBool ; TUnknown ; TChan (TBool) ]) ;;

print_endline (string_of_valueType t1) ;;

let p1 = makePrimType "core/arith/int" "add" [ TInt ; TInt ] TInt ;;

print_endline (string_of_valueType p1) ;;

(* values *)

print_endline "----------------- VALUES -----------------" ;;

print_endline (string_of_value (makeVTrue ()));;

print_endline (string_of_value (makeVFalse ()));;

print_endline (string_of_value (makeVInt 42));;

print_endline (string_of_value (makeVString "hello")) ;;

let t1' = makeTuple [ TInt ; TBool ; TInt ] [ (makeVInt 42) ; (makeVTrue ()) ; (makeVInt (-1)) ];;

print_endline (string_of_value t1') ;;

print_endline (string_of_value (makeVVar TInt "v")) ;;

let p1' = makeVPrim "core/arith/int" "add" [ TInt ; TInt ] TInt [ (makeVInt 42) ; (makeVInt 42) ] ;;

print_endline (string_of_value p1') ;;

(* actions *)

print_endline "----------------- ACTIONS -----------------" ;;

print_endline (string_of_action (makeTau ())) ;;

let out1 = makeOutput "c" (makeVInt 42) TInt ;;

print_endline (string_of_action out1) ;;

let in1 = makeInput "c" "x" TInt ;;

print_endline (string_of_action in1) ;;

let new1 = makeNew "c" TInt ;;

print_endline (string_of_action new1) ;;

let new2 = makeNew "d" (TChan TInt) ;;

print_endline (string_of_action new2) ;;

let spawn1 = makeSpawn "" "PingPong" [(TChan TInt);(TChan TInt)] [ (makeVVar TUnknown "o") ; (makeVVar TUnknown "i") ] ;;

print_endline (string_of_action spawn1) ;;

let prim1 = makePrim "core/io/console" "echo" [TUnknown] [ makeVVar TInt "b" ] ;;

print_endline (string_of_action prim1) ;;

let let1 = makeLet "x" TInt (makeVPrim "core/arith/int" "add" [TInt; TInt] TInt [makeVVar TInt "a" ; makeVInt 42]) TUnknown ;;

print_endline (string_of_action let1) ;;

(* processes *)

print_endline "----------------- PROCESSES -----------------" ;;

let term1 = makeTerm "Test" "def1" ;;

print_endline (string_of_process term1) ;;

let call1 = makeCall "Test" "def1" "Test" "def2" [ TInt; TBool ] [ makeVInt 42 ; makeVTrue () ] ;;

print_endline (string_of_process call1) ;;

let choice1 = makeChoice "Test" "def1" [ (makeVTrue (), TBool, makeTau (), makeTerm "Test" "def1") ] ;;

print_endline (string_of_process choice1) ;;

let choice2 = makeChoice "Test" "def1" [ (makeVPrim "core/arith/int" "equal" [ TInt ; TInt ] TBool [ makeVVar TInt "a" ; makeVInt 42], TBool, 
                                          (makeOutput "c" (makeVVar TInt "a") TInt),
                                          makeTerm "Test" "def1") ;
                                         (makeVTrue (), TBool, 
                                          makeInput "c" "x" (TChan TInt),
                                          (makePrefix "Test" "def1" (makeOutput "x" (makeVVar TInt "a") TInt)
                                             (makeTerm "Test" "def1"))) ] ;;
                                            
print_endline (string_of_process choice2) ;;


(* parser tests *)

print_endline "----------------- PARSING -----------------" ;;

let proc1 = ParseUtils.parseProcessFromString "c?(x) , c!x , end" ;;

print_endline (string_of_process proc1) ;;

let ppstr = "def PingPong(i:chan<string>,o:chan<string>,msg:string) = i?(m), #core/io:println(m), o!msg, PingPong(i,o,msg)"

let ppdef = ParseUtils.parseDefinitionFromString ppstr  ;;

print_endline (string_of_definition ppdef) ;;

let pp = ParseUtils.parseFromString ("module Test/PingPong \n" ^ ppstr ^ "\n def Main() = new(c1:chan<string>),new(c2:chan<string>),spawn{PingPong(c1,c2,\"<PING>\")},spawn{PingPong(c2,c1,\"<PONG>\")},c1!\"<INIT>\",end") ;;

print_endline (string_of_module pp) ;;

let fibStr = "def Fib(n:int,m:int,p:int,r:chan<int>)=[n]r!m,end+tau,Fib(n,m,m,r)";;
(* le parser n'accepte pas la garde n=0 ni n-1 et m+p !! introduire les expressions arithmétiques ??
   
   let fibStr = "def Fib(n:int,m:int,p:int,r:chan<int>)=[n=0]r!m,end+tau,Fib(n-1,m+p,m,r)";; *)

let fibdef = ParseUtils.parseDefinitionFromString fibStr;;

print_endline (string_of_definition fibdef) ;;
