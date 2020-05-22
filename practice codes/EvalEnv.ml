(*************************************************)
(* An environment-based evaluator for Dynamic ML *)
(*************************************************)

open Syntax
open Printing
open EvalUtil

(* Defines the subset of expressions considered values
   Notice that closures are values but the rec form is not -- this is
   slightly different from the way values are defined in the 
   substitution-based interpreter.  Rhetorical question:  Why is that?
   Notice also that Cons(v1,v2) is a value (if v1 and v2 are both values).
*) 
let rec is_value (e:exp) : bool = 
  match e with
      Constant _ -> true  
    | Pair (e1, e2) -> is_value e1 && is_value e2
    | EmptyList -> true
    | Cons (e1, e2) -> is_value e1 && is_value e2
    | Closure _ -> true
    | _ -> false

(* evaluation; use eval_loop to recursively evaluate subexpressions *)
let eval_body (env:env) (eval_loop:env -> exp -> exp) (e:exp) : exp = 
  match e with
  | Var x -> (
    match lookup_env env x with 
      | None -> raise (UnboundVariable x)
      | Some v -> eval_loop env v
  )
  | Constant _ -> e
  | Op(e1, op, e2) -> (
    let v1 = eval_loop env e1 in 
    let v2 = eval_loop env e2 in 
      apply_op v1 op v2 
  )
  | If(e1,e2,e3) -> (
    match eval_loop env e1 with 
      | Constant (Bool true) -> eval_loop env e2
      | Constant (Bool false) -> eval_loop env e3
      | v1 -> raise (BadIf v1)
  )
  | Let(v, e1, e2) -> (
    let e1 = (
      match e1 with 
      | Rec(f,x,b) -> Closure(env, f, x, b)
      | _ -> (
        match eval_loop env e1 with
        | Rec(f,x,b) -> Closure(env, f, x, b)
        | e1 -> e1
      )
    ) in
    let env = update_env env v e1 in
    let e2 = (
      match e2 with 
      | Rec(f,x,b) -> Closure(env, f, x, b)
      | _ -> (
        match eval_loop env e2 with
        | Rec(f,x,b) -> Closure(env, f, x, b)
        | e2 -> e2
      )
    ) in
    eval_loop env e2
  )
  | Pair(e1, e2) -> Pair(eval_loop env e1, eval_loop env e2)
  | Fst(e) -> (
    match eval_loop env e with
    | Pair(e1, e2) -> e1
    | _ -> raise (BadPair e)
  )
  | Snd(e) -> (
    match eval_loop env e with
    | Pair(e1, e2) -> e2
    | _ -> raise (BadPair e)
  )
  | EmptyList -> e 
  | Cons(e1, e2) -> Cons(eval_loop env e1, eval_loop env e2)
  | Match (e1, e2, hd, tl, e3) -> (
    match eval_loop env e1 with
    | EmptyList -> eval_loop env e2
    | Cons(e1, e2) -> (
      let up_env_hd = update_env env hd e1 in
      let up_env_tl = update_env up_env_hd tl e2 in
      eval_loop up_env_tl e3 
    )
    | _ -> raise (BadMatch e1)
  )
  | Rec _ -> e
  | Closure _ -> e
  | App(e1, e2) -> (
    let e1 = eval_loop env e1 in
    let e2 = eval_loop env e2 in
    match e1  with 
    | Rec (f, x, b) -> (
      let up_env_f = update_env env f e1 in
      let up_env_x = update_env up_env_f x e2 in
      eval_loop up_env_x b
    )
    | Var x -> (
      match lookup_env env x with 
      | None -> raise (UnboundVariable x)
      | Some v -> eval_loop env (App(v, e2))
    )
    | Closure (e, f, x, b) -> (
      let new_env = e in
      eval_loop new_env (App(Rec(f,x,b), e2))
    )
    | _ -> raise (BadApplication e)
  )
  (* | _ -> failwith "unimplemented" *)


let eval e =
  let rec loop env e = eval_body env loop e in
  loop empty_env e

  (* let eval ev e =
  let rec loop env e = eval_body env loop e in
  loop ev e *)


(* evaluate closed, top-level expression e *)

(* 

let exp4 = Let("x", Constant (Int 3), Op(Var "x", Plus, Constant (Int 5)));;
let exp5 = Let("x", Op(Constant (Int 3), Plus, Constant (Int 2)), Op(Var "x", Plus, Constant (Int 5)));;
let exp6 = Let ("x", Fst (Var "p"), Let ("y", Snd (Var "p"), Pair(Var "y", Var "x")));;
let env6 = ["p", Pair(Constant (Int 1), Constant (Int 2))];;
*)  

(* print out subexpression after each step of evaluation *)
let debug_eval e = 
  let rec loop env e =
    if is_value e then e  (* don't print values *)
    else 
      begin
	Printf.printf "Evaluating %s with env is %s\n" (string_of_exp e) (string_of_env env); 
	let v = eval_body env loop e in 
	Printf.printf 
	  "%s evaluated to %s\n" (string_of_exp e) (string_of_exp v); 
	v
      end
  in
  loop empty_env e 
