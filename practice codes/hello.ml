
print_string "hello world\n";;

let rec sumTo (n:int) : int =
  match n with 0 -> 0
  | n -> n + sumTo (n-1)
  
let _ =
  print_int (sumTo 8); 
  print_newline()