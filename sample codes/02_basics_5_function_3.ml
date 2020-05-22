let add_one (x:int) : int = 1 + x;;

let add_two (x:int) : int = add_one (add_one x);;

let add (x:int) (y:int) : int = x + y;;


add_one (3+4);;
add_two (3*4);;


add 5 6;;

add (3+4);;

(add (3+4)) 7;;

add (3+4) 7;;
