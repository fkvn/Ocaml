(*** In-Class Activity 03 (Feb 05) ***)
(* Due: Feb 5 (Wednesday) 11:59 PM *)


(* Don't remove the following lines. *)
let undefined : unit -> 'a = fun () -> failwith "undefined";;
 
let rec map (f:'a -> 'b) (xs: 'a list) : 'b list =
  match xs with
    | [] -> []
    | hd::tl -> (f hd) :: (map f tl);;
;;

let rec reduce (f:'a->'b->'b) (base:'b) (xs : 'a list) : 'b =
  match xs with
    | [] -> base
    | hd :: tl -> f hd (reduce f base tl)
;;




(* Problem 1. *)
(* 1.a. Using map, write a function that takes a list of pairs
   of integers, and produces a list of the sums of the pairs. 

   e.g., list_add [(1,3); (4,2); (3,0)] = [4; 6; 3]
*)
let list_add (lst:(int*int) list) : int list =
  map (fun (x, y) -> x + y) lst
;;

assert (list_add [(1,3); (4,2); (3,0)] = [4; 6; 3]);;



(* 1.b. Write list_add directly using reduce.
*)
let list_add_reduce (lst:(int*int) list) : int list =
  reduce (fun (x, y) b -> (x + y)::b ) [] lst
;;

assert (list_add_reduce [(1,3); (4,2); (3,0)] = [4; 6; 3]);;




(* Problem 2. *)
(* Using reduce, write a function that takes a list of optional integers,
   and filters out all of the None’s.

   e.g., filter_none [Some 0; Some 2; None; Some 1] = [0;2;1]
*)
let filter_none (lst:int option list) : int list =
  reduce (
    fun x b ->
    match x with
     | Some v -> v :: b
     | None -> b
  ) [] lst
;;

assert (filter_none [Some 0; Some 2; None; Some 1] = [0;2;1]);;
