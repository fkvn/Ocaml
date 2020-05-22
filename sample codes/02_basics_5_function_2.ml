let add_two (x:int) : int = 
  let add_one x = 1 + x in
    add_one (add_one x)
;;


add_two 5;;
add_one 5;;
