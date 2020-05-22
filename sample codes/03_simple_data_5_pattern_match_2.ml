type point = float * float;;

let distance (p1:point) (p2:point) : float = 
  let square x = x *. x in
    match p1 with
      | (x1,y1) ->
          match p2 with
            | (x2,y2) ->
                sqrt (square (x2 -. x1) +.
                      square (y2 -. y1))
;;



let pt1 = (3.0, 5.0);;
let pt2 = (0.0, 1.0);;
let dist12 = distance pt1 pt2;;

