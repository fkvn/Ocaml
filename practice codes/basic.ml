let add (x:int) (y:int) : int = 
  x + y
;;

print_int (add 2 3);;
print_newline();;

let y = let x = 2 + 3 in x * x;;
print_int y;;
print_newline();;