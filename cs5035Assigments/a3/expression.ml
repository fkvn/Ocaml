(*** CS5035 Assignment 3 Part 2: A Language for Symbolic Differentiation [75 POINTS] ***)

(* Make sure that your submission does NOT have
 * both compiling errors and running errors. *)

(* You are free to use any function from the Pervasives module (http://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html).
 *
 * You should NOT use functions from the Float module (https://caml.inria.fr/pub/docs/manual-ocaml/libref/Float.html).
*)

(* You are free to define auxiliary functions.
 * Just make sure that if we name a particular function that
 * you have to write (either in the assignment text, or in a 
 * template file), that you preserve its name so that our 
 * automated grader can find it.
*)

(* TIPS FOR PART 2:
 * 1. Read the writeup.
 * 2. Use the type definitions in the ast.ml as a reference. But don't worry 
 *    about expressionLibrary.ml
 * 3. Test!  (Use "assert" where appropriate.)
*) 

open Ast;;
open ExpressionLibrary;;

let undefined (s:string) : 'a = failwith (s^" undefined");;
let unimplemented (s:string) : 'a = failwith (s^" unimplemented");;
(* ====== Don't remove the above lines. ====== *)




(*>* Problem 2.1 [5 POINTS] *>*)
(* contains_var : tests whether an expression contains a variable "x"
 *     Examples : contains_var (parse "x^4") = true
 *                contains_var (parse "4+3") = false 
 *
 * parse is a function with type string -> expression (defined in expressionLibrary.ml)
*)
let rec contains_var (e:expression) : bool =
  match e with
    | Num n -> false
    | Var -> true
    | Unop (u,e1) -> contains_var e1
    | Binop (b,e1,e2) -> contains_var e1 || contains_var e2
;;

assert (contains_var (parse "x^4") = true);;
assert (contains_var (parse "4+3") = false);;




(*>* Problem 2.2 [10 POINTS] *>*)
(* evaluate : evaluates an expression for a particular value of x. Use OCaml's
 *            built-in function (i.e. function (/.) ) of handling 'divide by zero' errors.
 *  Example : evaluate (parse "x^4 + 3") 2.0 = 19.0 *)
(* Hint: Use OCaml's built-in functions sin, cos, log, (~-.) to
 *       evaluate unary operators Sin, Cos, Ln, Neg, respectively.
 * See the Pervasives module for details:*
 * http://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html
*)
let rec evaluate (e:expression) (x:float) : float =
  match e with
    | Num n -> n
    | Var -> x
    | Unop (u,e1) -> (match u with
        | Sin -> sin (evaluate e1 x)
        | Cos -> cos (evaluate e1 x)
        | Ln -> log (evaluate e1 x)
        | Neg -> ~-. (evaluate e1 x)
      )
    | Binop (b,e1,e2) -> (match b with
        | Add -> (evaluate e1 x) +. (evaluate e2 x)
        | Sub -> (evaluate e1 x) -. (evaluate e2 x)
        | Mul -> (evaluate e1 x) *. (evaluate e2 x)
        | Div -> (evaluate e1 x) /. (evaluate e2 x)
        | Pow -> (evaluate e1 x) ** (evaluate e2 x)
    )
;;

assert (evaluate (parse "x^4 + 3") 2.0 = 19.0);;

(*>* Problem 2.3 [20 POINTS] *>*)
(* See writeup for instructions.  *)
let rec derivative (e:expression) : expression =
  match e with 
  | Num n -> Num 0.
  | Var -> Num 1.
  | Unop (u, e1) -> (match u with
    | Sin -> Binop (Mul, derivative e1, Unop (Cos, e1))
    | Cos -> Binop (Mul, derivative e1, Unop(Neg, Unop (Sin, e1)))
    | Ln -> Binop (Div, derivative e1, e1)
    | Neg -> Unop (Neg, (derivative e1))
  )
  | Binop (b, e1, e2) -> (match b with
    | Add -> Binop (Add, derivative e1, derivative e2)
    | Sub -> Binop (Sub, derivative e1, derivative e2)
    | Mul -> Binop (Add, 
                    Binop (Mul, derivative e1, e2), 
                    Binop (Mul, e1, derivative e2))
    | Div -> Binop (Div, 
                    Binop (Sub, Binop (Mul, derivative e1, e2) ,
                    Binop (Mul, e1, derivative e2)) ,Binop (Pow, e2, e2))
    | Pow -> 
      if not (contains_var e2) then 
        Binop (Mul, 
              Binop (Mul, e2, derivative e1),
              Binop (Pow, e1, Binop (Sub, e2, Num 1.)))
      else
        Binop (Mul, Binop (Pow, e1, e2),
              Binop (Add, 
                    Binop (Mul, derivative e2, Unop(Ln, e1)),
                    Binop (Div, Binop (Mul, derivative e1, e2), e1)
                    )
              )

  )
;;

(* derivate (parse "x^4 + 3") *)



(* A helpful function for testing. See the writeup. *)
let checkexp strs xval=
  print_string ("-------------------------------\n");
  print_string ("Checking expression: " ^ strs^"\n");
  let parsed = parse strs in (
    print_string "contains variable: ";
    print_string (string_of_bool (contains_var parsed));
    print_endline " ";
    print_string "Result of evaluation: ";
    print_float  (evaluate parsed xval);
    print_endline " ";
    print_string "Result of derivative: ";
    print_endline " ";
    print_string (to_string_smart (derivative parsed));
    print_endline " ")
;;

(* === Testing Cases === *)
checkexp "x^4 + 3" 2.0;;
checkexp "x^2 + 3" 2.0;;
checkexp "4+8" 3.0;;
(* Come up your own test cases here. *)


(*>* Problem 2.4 [10 POINTS] *>*)
(* See writeup for instructions. *)
let rec find_zero (e:expression) (g:float) (epsilon:float) (lim:int)
  : float option = 
  if lim < 0 then None 
  else 
    let absFx = abs_float (evaluate e g) in
    if absFx < epsilon then Some g
    else 
      let e' = derivative e in
      let newG = g -. ( (evaluate e g) /. (evaluate e' g) ) in
      find_zero e newG epsilon (lim - 1) 
;;


(*>* Problem 2.5 [30 POINTS] *>*)
(* See writeup for instructions. *)
(* For this problem, we assume the input expression ONLY contains
 * Add, Sub, Mul, and Neg operators (nested arbitrarily). *)


let rec sim_e (e: expression) = 
  match e with
  | Num n -> [(n, 0.)]
  | Var -> [(1., 1.)]
  | Unop (u, e1) -> ( match u with
      | Neg -> let xs = sim_e e1 in List.map (fun (c,d) -> (~-.c, d)) xs 
      | u -> invalid_arg "Only Neg is allowed for Unary Operation"
    )
  | Binop (b, e1, e2) -> match b with
    | Add | Sub -> (
        let left = sim_e e1 in 
        let right = 
          if b = Add then sim_e e2 
          else List.map (fun (rc, rd) -> (rc *. (-1.), rd)) (sim_e e2)
        in
        let rs = List.fold_left (fun x y -> y :: x) [] right in
        List.fold_left (
          fun rs (lc, ld) ->
            let exist = 
              List.fold_left (
                fun b (rc, rd) -> if ld = rd then b || true else b
              ) false right 
            in
            if exist = true then
              List.map (
                fun (rc, rd) -> 
                  if ld = rd then (lc +. rc, rd) 
                  else (rc, rd)
              ) rs
            else (lc, ld) :: rs 
        ) rs left
      )
    | Mul -> 
      let left = sim_e e1 in
      let right = sim_e e2 in
      List.fold_left (
        fun rs (lc, ld) ->
          List.fold_left (
            fun rs (rc, rd) -> 
              let (c, d) = 
                if ld = 0. && rd = 0. then (lc *. rc, 0.)
                else if ld > 0. && rd > 0. then (lc *. rc, rd +. 1.)
                else (lc *. rc, 1.)
              in 
              let existInRs = 
                List.fold_left (
                  fun b (rsc, rsd) -> if d = rsd then b || true else b
                ) false rs 
              in
              if existInRs = true then
                List.map (
                  fun (rsc, rsd) -> 
                    if d = rsd then (c +. rsc, rd) 
                    else (rsc, rsd)
                ) rs
              else (c, d) :: rs
          ) rs right
      ) [] left
    | b -> invalid_arg "Only Add, Mul, Sub are allowed for Binary Operation"
;;

let rec find_zero_exact (e:expression) : expression option =
  let rs = sim_e e in 
  let nrs = List.fold_left (
      fun x (c, d) -> if c = 0. && d > 1. then x else  (c, d) :: x
    ) [] rs 
  in
  let ifDegree1 = 
    if (List.length nrs) > 2 then false
    else
      List.fold_left (
        fun b (c, d) -> 
          if d = 1. && c = 0. then b && false
          else b
      ) true nrs
  in 
  if not ifDegree1 then None
  else
    let sortedNrs = List.sort (fun (_, d) (_, d') -> 
                                if d < d' then -1 
                                else if d > d' then 1 
                                else 0)
                    nrs
    in
    match sortedNrs with
     | [] -> None
     | [(a1, _)] -> Some (Binop (Div, Unop (Neg, Num 0.), Num a1))
     | (a0, _) :: (a1, _) :: tl -> Some (Binop (Div, Unop (Neg, Num a0), Num a1))
;;


(* A helpful function to test finding zeros. See the writeup. *)
let check_zero str=
  print_string ("-------------------------------\n");
  print_string ("Checking zeros of " ^ str ^"\n");
  let parsed = parse str in (
    print_string "find_zero (initial guess 1.0, epsilon 0.0001, lim 100): ";
    let z = find_zero parsed (-5.0) 0.0001 6 in (
      match z with
        | None -> print_string "None"
        | Some f -> print_string (string_of_float f) );
      print_endline " ";
      print_string "find_zero_exact: ";
      let z' = find_zero_exact parsed in (
        match z' with
          | None -> print_string "None"
          | Some e -> print_string (to_string e) );
        print_endline " ")
;;

(* === Testing Cases === *)
check_zero "(3 * x + 1) * (4 * x + 2) - 12 * x * x";; (* 12x + 2 -> -2/10 *)
check_zero "x * x * x + 3 * x * x + 12 * x + 8";; (* x^3 + x^2 +12x + 8  -> *)
check_zero "(~6) * x";; (* -6x  -> -0 / -6 *)
check_zero "6 * x";;  (*  6x -> -0 / 6 -> *)
check_zero "3";; (*  3 + 0x  -> None *)
check_zero "(~3) * x  - 1";; (* -3x - 1 -> -(-1) / -3 *)
check_zero "3 * x  - 1";; (* 3x - 1 -> -(-1) / 3  -> *)
check_zero "3*x+(x+1)*(2-x)-x*x";; (*  -2x^2 + 4x + 2 -> None *)
check_zero "3*x+(x+1)*(2-x)+x*x";; (* 4x + 2 -> -2 / 4  -> *)
(* Come up your own test cases here. *)



(*>* [OPTIONAL] *>*)
(* For extra fun (but not extra credit),
   implement find_zero_exact_2 that solves degree-two expressions.
   This is almost as easy as solving degree-one expressions,
   if you use the quadratic formula.  Almost as easy, assuming
   you've already done the work to normalize polynomials into an
   easily recognizable form. *)
(*
let rec find_zero_exact_2 (e:expression) : string =
unimplemented "find_zero_exact_2"
;;
*)


(* A helpful function to test finding zeros (accept 2-degree expression). *)
(*
let check_zero_2 str=
print_string ("-------------------------------\n");
print_string ("Checking zeros (accept 2-degree expression) of " ^ str ^"\n");
let parsed = parse str in (
print_string "find_zero (initial guess 1.0, epsilon 0.0001, lim 100): ";
let z = find_zero parsed 1.0 0.0001 100 in (
match z with
| None -> print_string "None"
| Some f -> print_string (string_of_float f) );
print_endline " ";
print_string "find_zero_exact_2: \n";
print_string (find_zero_exact_2 parsed);
print_endline " ")
;;
*)


(* === Testing Cases === *)
(* You may come up with your own testging cases. *)
(* check_zero_2 "x";; *)
(* Come up your own test cases here. *)






(*>* [OPTIONAL] *>*)
(* For extra fun (but not extra credit), 

   Consider this function,
   let evaluate2 (e: expression) : float -> float =
   let e' = derivative e in
   fun x -> (evaluate e x, evaluate e' x)

   Such a function can be used in Newton's method.
   But if the expression e is large, e' can be exponentially larger,
   because of the chain rule for multiplication, so
   evaluate e' x  can be slow.

   One solution is called "forward mode automatic differentiation",
   which has become an important algorithm (since 2017 or so) in
   deep learning.  You can read about it in section 3.1 of
   this paper:  http://jmlr.org/papers/volume18/17-468/17-468.pdf
   "Automatic Differentiation in Machine Learning: A Survey"
   (and pay particular attention to Table 2 for a worked example).

   So, the challenge (which is actually not very difficult) is,
   write this function

   let evaluate2 (e: expression) (x: float) : float * float = ...

   that computes both e(x) and the first derivative e'(x),
   without ever calculating (derivative e).  Like evaluate,
   do it by case analysis on the syntax-tree of e. *)
	   
(* Q.  Why do it, if no extra credit?
   A.  Because (and only if) it's fun.  
   A.  Because CS5035 is a graduate course.
   A.  Because the main reason you're working so hard
   is to learn things, not just to get grades.
   A.  Any well educated computer scientist graduating after 2019
   ought to know something about deep learning . . .
*)




