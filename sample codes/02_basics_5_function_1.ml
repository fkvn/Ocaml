let add_one (x:int) : int = 1 + x;;

let add_two (x:int) : int = add_one (add_one x);;


add_one 5;;
add_two 5;;
