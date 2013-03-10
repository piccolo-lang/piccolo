(* module PrintUtils
   -----------

  Utilitaries for printing

*)
(** This module defines various printing methods. *)

open Format;;

(** Take a function ton print one element, a string separator and a list of elements and print each one with the separator in between *)
let rec print_list f sep fmt = function
  | [] -> ()
  | [x] -> f fmt x
  | h::t -> fprintf fmt "%a%s@ %a" f h sep (print_list f sep) t
;;

(** Same function but take two functions and print two list at a time*)
let rec print_list2 f1 f2 sep fmt l =
  match l with
    | [], [] -> ()
    | [], _ | _ , [] -> raise (Invalid_argument "List of different sizes")
    | [x], [y] -> fprintf fmt "%a %a" f1 x f2 y
    | (h1::t1, h2::t2) -> 
	fprintf fmt "%a %a%s@ %a" f1 h1 f2 h2 sep (print_list2 f1 f2 sep) (t1, t2)
;;

(** Print a list of element with a separator and a break line between each element.
    No break line after the last element.
 *)
let rec print_list_eol f sep fmt = function
  | [] -> ()
  | [x] -> fprintf fmt "%a%s" f x sep
  | h::t -> fprintf fmt "%a%s@\n%a" f h sep (print_list_eol f sep) t
;;

(** Same function but put a break line after the last element *)
let rec print_list_eol' f sep fmt = function
  | [] -> ()
  | h::t -> fprintf fmt "%a%s@\n%a" f h sep (print_list_eol' f sep) t
;;

(** string printer function to be used with the previous functions  *)
let print_string fmt str = fprintf fmt "%s" str;;
