type point = float * float;;

let slope (p1:point) (p2:point) : float = 
  let (x1,y1) = p1 in
  let (x2,y2) = p2 in
  let xd = x2 -. x1 in
    if xd <> 0.0 then
      (y2 -. y1) /. xd
    else
      None???
;;



let pt1 = (3.0, 5.0);;
let pt2 = (0.0, 1.0);;
slope pt1 pt2;;

let pt3 = (3.0, 3.0);;
slope pt1 pt3;;


