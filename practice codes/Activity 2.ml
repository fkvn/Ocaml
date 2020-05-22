(* (1) Write a function to append two lists
• – append [1;2;3] [4;5;6] ==> [1;2;3;4;5;6] • *)

let rec append_list (xs: int list) (ys: int list) : int list =
  match (xs, ys) with
  | ([], ys) -> ys
  | (hd' :: xs', ys') -> hd' :: (append_list xs' ys')
in
  append_list [1;2;3] [4;5;6];;


(* (2) Write a function to revers a list
• – rev [1;2;3] ==> [3;2;1] • *)

let rev (xs: int list) : int list =
  let rec rev_helper (xs: int list) (ys: int list) : int list =
    match (xs, ys) with
    | ([], ys) -> ys
    | (hd :: xs', ys) -> rev_helper xs' (hd :: ys)
  in 
    rev_helper xs []
in
  rev [1;2;3];;


(* (3) Write a function to convert a list of pairs into a pair of lists
• – split [(1,2); (3,4); (5,6)] ==> ([1;3;5], [2;4;6]) • *)

let convert (xs: (int * int) list): ((int list) * (int list)) =
  let rec convert_helper (xs: (int * int) list) (ys: ((int list) * (int list))) :  ((int list) * (int list)) =
    match (xs, ys) with
    | ([], (ys1, ys2)) -> (List.rev ys1, List.rev ys2)
    | ((x, y) :: tlxs, (ys1, ys2)) -> convert_helper tlxs ((x::ys1),(y::ys2))
  in
    convert_helper xs ([], [])
in
  convert [(1,2); (3,4); (5,6)];;

(* (4) Write a function that returns all prefixes of a list
• – prefixes [1;2;3] ==> [[]; [1]; [1;2]; [1;2;3]] *)

(* 
#let rec map f =
# function [] -> []
# | x::l -> (f x)::(map f l);;
map : (’a -> ’b) -> ’a list -> ’b list = <fun>

#map (function x -> x+1) [1;2;3;4;5];;
- : int list = [2; 3; 4; 5; 6] 
*)

let rec prefixes  = function
  | [] -> [[]]
  | x::tl   -> []::List.map (function tl' -> x :: tl') (prefixes tl)
in
  prefixes [1;2;3];;