(*** CS5035 Assignment 2 Part 1: Lists [100 POINTS] ***)

(* The assignment is due on February 26 (Wednesday) at 11:59 PM. *)

(* Zip all files and submit in CSNS.
 *
 * Make sure that your submission does NOT have
 * both compiling errors and running errors. *)

(* DO NOT USE MAP/REDUCE FOR THIS ASSIGNMENT!!! *)


(* Don't remove the following line. *)
let undefined (s:string) : 'a = failwith (s^" undefined");;
let unimplemented (s:string) : 'a = failwith (s^" unimplemented");;




(*************)
(* PROBLEM 1 [15 POINTS] *)
(*************)
(* For each part of problem 1, explain in the given string why the code
   will not typecheck, then follow the instructions for that part to
   change the code so that it typechecks while keeping the same values
   as intended by the erroneous code.
*)

(* Problem 1a [5 POINTS] - Give your explanation in exp1a and then fix the
   right-hand-side of the assignment to match the listed type. 
   (Do not change the left-hand-side.)
*)
let exp1a : string = "The problem is using comma between element.  
                      To make a list, each element is seperated by semicolon. 
                      Comma is use for tuple";;

let prob1a : int list = [1; 2; 3];;

(* Problem 1b [5 POINTS] - Give your explanation in exp1b and then fix the type
   of variable prob1b to match the type of the expression on the
   right-hand-side of the assignment. (Do not change the
   right-hand-side.)
*)

let exp1b : string = "We need the parenthesis as (string * int), 
                      so it will become a tuple list type ";;

let prob1b : (string * int) list = [("CS", 5035); ("CS", 3035)];; 

(* Problem 1c [5 POINTS] - Give your explanation in exp1c and then fix the
   right-hand-side of the expression to match the variable prob1c's
   listed type. 
   (Do not change the left-hand-side.)  
*)

let exp1c : string = "When we use ::, it treats as add the list [2.0; 3.0] 
                      as a head of the [4.0;5.0]. 
                      Error: result list is float type and [2.0; 3.0] is list type.
                      To fix it, I use @ to append 2 lists together.";;

let prob1c : float list = [2.0; 3.0] @ [4.0; 5.0];;



(*************)
(* PROBLEM 2 [20 POINTS] *)
(*************)

(* Fill in expressions to satisfy the following types: 
 *
 * NOTE: for option, list, and function types, you must 
 * provide a nontrivial answer. For a list that means a 
 * non-empty one; for an option type that means a Some 
 * construction; and for a function, that means using 
 * its arguments to generate the return value.
 * example problems:
 *   let x : int option = ???
 *   let y : int list = ???
 *   let f (x: int) (y: int) : int = ???
 * incorrect answers:
 *   let x : int option = None
 *   let y : int list = []
 *   let f (x: int) (y: int) : int = 7
 * possible correct answers:
 *   let x : int option = Some 1
 *   let y : int list = [1]
 *   let y : int list = [1; 2]
 *   let y : int list = 1 :: [2]
 *   let f (x: int) (y: int) : int = x + y
 *   let f (x: int) (y: int) : int = 
 *         String.length  ((string_of_int x) ^ (string_of_int y))
*)

(*>* Problem 2a [5 POINTS] *>*)

let prob2a : (float * (string * int) option list) list =
 [3.5, [Some ("prob2a", 5)]]
;;

(*>* Problem 2b [5 POINTS] *>*)
(* a student is a (name, age option) pair *)

type student = string * int option;;

let prob2b : (student list option * int) list = 
 [Some [("Kevin", Some 5)], 5]
;;  

(*>* Problem 2c [5 POINTS] *>*)

let prob2c : (int * int -> int) * (float -> float -> unit) * bool  = 
 ((fun (x,y) -> x + y), (fun (x:float) (y:float) -> print_float (x +. y)), true)
;;

(*>* Problem 2d [5 POINTS] *>*)
(* Fill in ??? with something to make prob2d typecheck *)

let prob2d =
 let rec foo bar =
  match bar with
  | (a, (b, c)) :: xs -> if a then (b + c + (foo xs)) else foo xs
  | _ -> 0
 in
  foo [true, (2, 5); false, (2, 3)]
;;



(*************)
(* PROBLEM 3 [10 POINTS] *)
(*************)

(* Consider the following terribly written function: *)

let rec zardoz f ls acc =
 if (((List.length (ls@[])) = 1) = true) then (f (List.hd(ls)) (acc))
 else if (((List.length ls) = 0) = true) then acc
 else
  let hd = List.hd(ls) in
  let tl = List.tl(ls) in
  let ans = f (hd) (acc) in
  let ans = zardoz f tl ans in ans
;;

assert (zardoz (fun x y -> x + y) [1;5] 5 = 11);;
assert (zardoz (fun x y -> x + y) [] 5 <> 6);;

(* Rewrite the code above so that it does the same thing
 * but style-wise is far superior.  
 * Be sure to provide types for the function's arguments and to 
 * call itself (myzardoz NOT the original zardoz) recursively as needed.
 * You may want to write some assert statements
 * to check that your function is doing the same thing as zardoz.  
 * Use the style guide (http://cs3.calstatela.edu/~cguo/CS5035/OCaml_Style_Guide.html). *)

let rec myzardoz f ls acc = 
 match ls with
 | [] -> acc
 | hd :: tl -> 
   match tl with
   | [] -> f hd acc
   | tl -> myzardoz f tl (f hd acc)
;;

assert (myzardoz (fun x y -> x + y) [1;5] 5 = 11);;
assert (myzardoz (fun x y -> x + y) [] 5 <> 6);;


(*************)
(* PROBLEM 4 [15 POINTS] *)
(*************)

(***************************************)
(* Conway's Lost Cosmological Theorem! *)
(***************************************)

(* 
If l is any list of integers, the look-and-say list of l is obtained by 
reading off adjacent groups of identical elements in l. For example, the 
look-and-say list of

l = [2; 2; 2]

is

[3; 2]

because l is exactly "three twos". Similarly, the look-and-say sequence of

l = [1; 2; 2]

is

[1; 1; 2; 2]

because l is exactly "one ones, then two twos".

You will now define a function look_and_say that computes the 
look-and-say sequence of its argument. look_and_say of an empty 
list is the empty list. 

For full credit your solution should be a LINEAR TIME solution.

==================================== 
CULTURAL ASIDE:

The title of this problem comes from a theorem about the sequence generated 
by repeated applications of the "look and say" operation. As look and say 
has type int list -> int list, the function can be applied to its own result. 
For example, if we start with the list of length one consisting of just the 
number 1, we get the following first 6 elements of the sequence:

[1]
[1,1]
[2,1]
[1,2,1,1]
[1,1,1,2,2,1]
[3,1,2,2,1,1]

Conway's theorem states that any element of this sequence will "decay" 
(by repeated applications of look and say) into a "compound" made up of 
combinations of "primitive elements" (there are 92 of them, plus 2 
infinite families) in 24 steps. If you are interested in this sequence, 
you may wish to consult [Conway(1987)] or other papers about the 
"look and say" operation.

==================================== 
===YOU ARE NOT REQUIRED TO IMPLEMENT THE FUNCTION RUNS!!!===
Progamming practice aside related to the "look and say" problem. You
may find this useful for constructing your solution to "look and say",
or you may not. 

Another interesting list problem is determining "runs"
in a list: maximal length sublists with all equal elements. For
example,

[1; 1; 1] and [5]

are both runs of the list

[1; 1; 1; 5; 2]

but

[1; 1] and [5; 2] and [1; 2]

are not: 

[1; 1] is not maximal
[5; 2] has unequal elements
[1; 2] is not a sublist.
*)

(* 
look_and_say [1]
look_and_say [1;1]
look_and_say [2;1]
look_and_say [1;2;1;1]
look_and_say [1;1;1;2;2;1]
look_and_say [3;1;2;2;1;1] *)

(* from activity 2 *)
let rev xs =
 let rec rev_helper xs ys =
  match (xs, ys) with
  | ([], ys) -> ys
  | (hd :: xs', ys) -> rev_helper xs' (hd :: ys)
 in 
  rev_helper xs []
;;

let look_and_say (xs: int list) : int list = 
 let rec aux xs rs = 
  match xs with
  | [] -> rev rs
  | hd :: tl -> 
    match rs with 
    | [] -> aux tl ([hd; 1])
    | x::y::tl' when hd = x -> aux tl (x :: y + 1 :: tl')
    | _ -> aux tl (hd :: 1 ::rs)
 in
  aux xs []
;;

assert (look_and_say [] = []);;
assert (look_and_say [2;2;2] = [3;2]);;
assert (look_and_say [1;2;2] = [1;1;2;2]);;
assert (look_and_say [1;2;2;1] = [1;1;2;2;1;1]);;


(* look_and_say [1;1;2;2;1;1];; -> 1,1,2,2,2,1 *)

(*************)
(* PROBLEM 5 [10 POINTS] *)
(*************)

(* Write a function that flattens a list of lists in to a single
 * list with all of the elements in the same order they appeared in 
 * the original list of lists. eg:
 *
 * flatten [[1;2;3]; []; [4]; [5;6]] = [1;2;3;4;5;6] 
 * flatten [[]; ['e';'d']; ['a';'b';'c']] = ['e';'d';'a';'b';'c'] 
*)


(* from activity 2 *)
let rec append_list xs ys =
 match (xs, ys) with
 | ([], ys) -> ys
 | (hd' :: xs', ys') -> hd' :: (append_list xs' ys')
;;

let rec flatten (xss:'a list list) : 'a list =
 let rec aux xss xs = 
  match (xss, xs) with
  | ([], xs) -> xs
  | (hd::tl, xs) -> aux tl (append_list xs hd)
 in 
  aux xss []
;;

assert (flatten [[]] = []);;
assert (flatten [[1;2;3]; []; [4]; [5;6]] = [1;2;3;4;5;6]);;
assert (flatten [[]; ['e';'d']; ['a';'b';'c']] = ['e';'d';'a';'b';'c']);;
 



(*************)
(* PROBLEM 6 [30 POINTS] *)
(*************)

(* [25 POINTS]
   Return the list of all permutations of the input list. eg: 
   perm [1;2;3] = [[1;2;3]; [1;3;2]; [2;1;3]; [2;3;1]; [3;1;2]; [3;2;1]] 
   The ordering of the permutations does not matter in your solution.
   We will accept either [[]] or [] as the result for perm [].
   NB: test this on small inputs - perm is ~n! which is approximately ~n^n.
*)

let len l = 
 let rec aux lt n =
  match lt with 
  | [] -> n
  | hd :: tl -> aux tl (n + 1)
 in 
  aux l 0
;;

let rec distribute ele ls = 
 match ls with
 | [] -> []
 | hd :: tl -> (ele :: hd) :: (distribute ele tl)
;;

let rec subsitute l x = 
 match l with
 | [] -> []
 | h :: t -> if h = x then t else h :: subsitute t x
;;

let rec get_ele_nth ls i nth = 
 if nth < 0 then raise (Failure "Invalid nth")
 else
  match ls with 
  | [] -> raise (Failure "get_ele_nth")
  | hd :: tl ->
    if i = nth then hd
    else get_ele_nth tl (i + 1) nth 
;;

(* perm (a) -> a 
   perm (ab) -> a + perm(b) && b + perm (a) 
   ....
*)

let perm (items:'a list) : 'a list list =
 let rec perm_aux items = 
  match items with 
  | [] -> []
  | [_] -> [items]
  | hd :: tl ->
    let rec aux index rs = 
     let ele = get_ele_nth items 1 index in
     let subP = perm_aux (subsitute items ele) in
     let disSubP = distribute ele subP in
      if (index < (len items)) then aux (index + 1) (append_list disSubP rs)
      else append_list disSubP rs
    in 
     let rs = aux 1 [] in rev rs
 in
  perm_aux items 
;;

(* [5 POINTS]
   Define the factorial function, then try pre-defined testperm.
   This question is to verify perm is ~n!
*)

let rec fac (x:int) : int =
 if x <= 1 then 1
 else x * fac (x - 1)
;;

let testperm al =
  assert (List.length (perm al) = fac (List.length al))
;;

testperm [1;2;3];;

