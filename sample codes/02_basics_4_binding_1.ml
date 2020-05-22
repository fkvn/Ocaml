let x = 3;;
let add_three (y:int) : int = y + x;;

let x = 4;;
let add_four (y:int) : int = y + x;;

let add_seven  (y:int) : int = 
  add_three (add_four y)
;;



add_three 5;;
add_four 5;;
add_seven 5;;
