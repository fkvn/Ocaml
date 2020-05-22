(* constraint (assumption): 1 <= n <= 9 *)
let small_prime (n:int) : bool =
  match n with
    | 2 -> true
    | 3 -> true
    | 5 -> true
    | 7 -> true
    | _ -> false
;;



small_prime 1;;
small_prime 2;;
small_prime 3;;
small_prime 4;;
small_prime 5;;
small_prime 6;;
small_prime 7;;
small_prime 8;;
small_prime 9;;

