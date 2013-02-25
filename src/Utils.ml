(*----------------------------------------------------------------------------    
  Utils.ml  Copyright (C) 2012 Frederic Peschanski
  This program comes with ABSOLUTELY NO WARRANTY; for details see `LICENSE'.
  This is free software, and you are welcome to redistribute it
  under certain conditions; see `LICENCE' for details.
  ----------------------------------------------------------------------------*)
(** This module defines various utility functions used in other parts of the compiler. *)

module SMap = Map.Make(String)

(* printing functions *)

(** convert a collection to a string. *)
let string_of_collection (op : string) (cl : string) (sep : string) (tostr : 'a -> string) (lst : 'a list) = 
  let rec str = function
    | [] -> ""
    | e::[] -> tostr e
    | e::es -> (tostr e) ^ sep ^ (str es)
  in
    op ^ (str lst) ^ cl
      
(** convert a list to a string. *)
let string_of_list tostr lst = string_of_collection "[" "]" ";" tostr lst
  
(** convert a set (represented as a list) to a string. *)
let string_of_set tostr lst = string_of_collection "{" "}" "," tostr lst
  
(* list utilities *)
  
(** return the last element of a given list *)
let rec last = function
  | [] -> failwith "last"
  | e::[] -> e
  | _::r -> last r

(** return a given list without its last element *)
let rec front = function
  | [] -> failwith "front"
  | e::[] -> []
  | e::r -> e::(front r);;

(** test if a list is empty *) 
let empty_list = function
  | [] -> true
  | _ -> false
      
(** type unit forget the argument. *)
let forget e = ()
  
(** either/Or sum type. *)
type ('a, 'b) either =
  | Left of 'a
  | Right of 'b
      
(** variant of iter passing the index of the current element in the list. *)
let list_iter_n f es = 
  let rec aux n es =
    match es with
      | [] -> ()
      | e::es' -> (f n e); aux (n + 1) es'
  in aux 0 es
       
(** variant of fold passing the index of the current element in the list. *)
let list_fold_n (f : 'a -> int -> 'b -> 'a) (e : 'a) (es : 'b list) =
  let rec aux index es rs =
    match es with
      | [] -> rs
      | e'::es' -> (aux (index + 1) es' (f rs index e'))
  in
    aux 0 es e
      
(** get the maximum integer of the specified list. *)
let list_max ns = List.fold_left (fun m n -> if m > n then m else n) min_int ns
  
  
  
