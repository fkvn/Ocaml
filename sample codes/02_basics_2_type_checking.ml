(2 + 3) * 5;;

let add_one (x:int) : int = 1 + x;;
7 * (add_one 17);;

3 + 1;;

"hello" ^ "world";;

(* "hello" + 1;; *)

"hello" ^ (string_of_int 1);;

if true then 7 else 8;;

(* if false then "1" else 2;; *)

(* 3 / 0;; *)

4.0 /. 0.0;;

(*
let rec concatn s n =
if n <= 0 then
0
else
s ^ (concatn s (n-1))
;;
*)

let rec concatn (s:string) (n:int) : string =
  if n <= 0 then
    0
  else
    s ^ (concatn s (n-1))
;;


