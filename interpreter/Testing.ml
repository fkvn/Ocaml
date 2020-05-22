(* Testing *)

(* In this file, we build abstract syntax trees representing various
   functions for the purpose of testing our evaluators.

   You will have to construct some additional functions yourself.
 *)

open Syntax
open Printing

(************)
(* INTEGERS *)
(************)

(* Useful Constants *)
let zero = Constant (Int 0) 
let one = Constant (Int 1) 
let two = Constant (Int 2) 
let three = Constant (Int 3) 
let four = Constant (Int 4)

(***********************)
(* RECURSIVE FUNCTIONS *)
(***********************)

(* let z = 2 in (let x = 3 in fun y -> x + y + z) (let x = 4 in x + z) *)
let clo =  
  Let ("z", two,
    App (
      Let ("x", three, 
          Rec ("f", "y", Op (Var "x", Plus, Op (Var "y", Plus, Var "z")))
      ),
      Let ("x", four, Op (Var "x", Plus, Var "z"))
    )
  )


(* (fun x -> let f = fun y -> y + x in let x = 3 in f x) 10 *)
let clo2 = 
  App (
    Rec ("f1", "x", 
      Let ("f", Rec ("f2", "y", Op(Var "y", Plus, Var "x")),
        Let ("x", three, App(Var "f", Var "x"))
      )
    )
    ,
    Constant (Int 10)
  )

(* rec fact n = if n < 1 then 1 else n * fact (n - 1) *)
let fact = 
  Rec ("fact", "n", 
       If (Op (Var "n", Less, one),
           one,
           Op (Var "n", Times, 
               App (Var "fact", 
                        Op (Var "n", Minus, one)))))

(* fact 4 *)
let fact4 = App (fact, four)

(*********)
(* PAIRS *)
(*********)

(* the pair (1,2) *)
let p1 = Pair (one, two)

(* the function swap below is equivalent to:
       let swap p = let (x,y) = p in (y,x)
*)
let swap = 
  Rec ("swap", "p",
       Let ("x", Fst (Var "p"),
       Let ("y", Snd (Var "p"),
       Pair(Var "y", Var "x"))))

(* use swap to swap the elements of p1 *)
let swap_p1 = App (swap, p1)

(*********)
(* LISTS *)
(*********)

(* takes an OCaml list of expressions and generates a single expression
   representing the list
*)
let rec listify (l:exp list) : exp =
  match l with
      [] -> EmptyList
    | hd::tl -> Cons(hd,listify tl)

(* a list of 4 numbers *)
let list4 = listify [one;two;three;four] 

(* rec sumlist l = 
 *   match l with
 *     [] -> 0
 *   | hd::tl -> hd + sumlist tl *)
let sumlist = 
  Rec ("sumlist", "l", 
       Match (Var "l",
           zero,
           "hd", "tl", Op (Var "hd", Plus, 
			   App (Var "sumlist", Var "tl"))))

let sl4 = App (sumlist, list4)
(* 
Match (Var "l",
           zero,
           "hd", "tl", Op (Var "hd", Plus, Constant (Int 5)));; *)
(*******************************)
(* QUESTIONS FOR YOU TO ANSWER *)
(*******************************)

(* NOTE: NONE OF THE FUNCTIONS YOU WRITE BELOW SHOULD INCLUDE
 * Closure (env,f,x,e)
 *
 * Define recursive functions using the Rec(f,x,body) form
 *)

(* Replace the constant "one" below with your implementation of 
   the function map : ('a -> 'b) -> 'a list -> 'b list 
   Note: do not implement this as map: (('a -> 'b)*'a list) -> 'b list
 *)

(* 
let rec map f = rec map xs = 
  match xs with 
  | [] -> []
  | x :: xs -> f x :: map f xs
*)
         
let map =
  Rec("map", "f",
    Rec("map", "xs",
      Match (Var "xs", EmptyList, 
        "hd", "tl", Cons(App(Var "f", Var "hd"), App(Var "map", Var "tl"))
      )
    )
  )

(* let testMap = App(App(map, plus1), list4) *)
  
(* Replace the constant "one" below with your implementation of 
   the function plus1 that adds one to an integer *)
let plus1 = Rec("f", "a", Op(Var "a", Plus, one))

(* Use plus1 and map, defined above, to implement the function 
   incr_all, which adds 1 to every element of a list. Examples:

   incr_all [] == []
   incr_all [1;2;3] == [2;3;4]
*)
let incr_all = 
  Rec("incr", "xs",
    App(App(map, plus1), Var "xs")
  )

let testIncr_all = App(incr_all, list4)
let testIncr_all_1 = App(incr_all, EmptyList)


(* Replace the constant one below by implementing a function that 
 * takes a list of pairs of integers and returns a list of integers 
 * where each element of the list is the sum of the elements of the 
 * pairs.  Examples:

  sum_pairs [] == []
  sum_pairs [(1,2); (3,4)] == [3; 7]
 *)

let sumPair = 
  Rec("sumPair", "pair",
    Op(Fst(Var "pair"), Plus, Snd(Var "pair"))
  )

let listOfPair = Cons(Pair(one, two), Cons(Pair(three, four) , EmptyList))

let sum_pairs = 
  Rec("sum_pairs", "pair",
    App(App(map, sumPair), Var "pair")
  )

let testSumPairs = App(sum_pairs, listOfPair)
let testSumPairs_1 = App(sum_pairs, EmptyList)

(*********)
(* TESTS *)
(*********)

(* Feel free to add many more tests of your own to the list *)
let tests = [zero; fact4; list4; sl4; clo; clo2;testIncr_all; testIncr_all_1;testSumPairs; testSumPairs_1]

let run_test eval exp =
  Printf.printf "========\n";
  Printf.printf "%s\n" (string_of_exp exp);
  Printf.printf "Evaluates to:\n";
  Printf.printf "%s\n" (string_of_exp (eval exp));
  Printf.printf "========\n"

let run_tests eval tests =
  List.iter (run_test eval) tests


    
      
