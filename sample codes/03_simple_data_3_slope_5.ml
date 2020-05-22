type point = float * float;;

let slope (p1:point) (p2:point) : float option = 
  let (x1,y1) = p1 in
  let (x2,y2) = p2 in
  let xd = x2 -. x1 in
    if xd <> 0.0 then
      Some ((y2 -. y1) /. xd)
    else
      None
;;

let print_slope (p1:point) (p2:point) : unit = 
  match slope p1 p2 with
    | Some s -> print_endline ("Slope: " ^ string_of_float s)
    | None -> print_endline "Vertical line "
;;



let pt1 = (3.0, 5.0);;
let pt2 = (0.0, 1.0);;
print_slope pt1 pt2;;

let pt3 = (3.0, 3.0);;
print_slope pt1 pt3;;


