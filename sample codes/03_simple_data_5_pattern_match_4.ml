type point = float * float;;

let distance (p1:point) (p2:point) : float = 
  let square x = x *. x in
    match (p1, p2) with
      | (p3, p4) ->
          let (x1,y1) = p3 in
          let (x2,y2) = p4 in
            sqrt (square (x2 -. x1) +.
                  square (y2 -. y1))
;;



let pt1 = (3.0, 5.0);;
let pt2 = (0.0, 1.0);;
let dist12 = distance pt1 pt2;;

