type point = float * float;;
type edge = point * point;;
(* directed graph *)
type graph = (point list) * (edge list);;

let distance (p1:point) (p2:point) : float = 
  let square x = x *. x in
  let (x1,y1) = p1 in
  let (x2,y2) = p2 in
    sqrt (square (x2 -. x1) +.
            square (y2 -. y1))
;;

(* return a nearby point in the graph if one exists
   nearby: (1) exist a edge links the two points
   (2) the return point is the NEXT of the input point
   (3)distance smaller than d
*)
let nearby (g:graph) (p:point) (d:float) : point option = 
  match g with
    | (pts, edges) ->
        let rec aux (es : edge list) : point option =
          match es with
            | [] -> None
            | hd :: tl ->
                ( match hd with
                  | (pt1, pt2) ->
                      ( if pt1 = p && (distance pt1 pt2)<d then
                          Some pt2
                        else
                          aux tl
                      )
                )
        in 
          aux edges
;;

let printer (g:graph) (p:point) (d:float) : unit =
  match nearby g p d with
    | None -> print_string "could not find one\n"
    | Some (x,y) ->
        print_float x;
        print_string ", ";
        print_float y;
        print_newline()
;;



let d = 2.5;;
let pt1 = (3.0, 5.0);;
let pt2 = (0.0, 1.0);;
let pt3 = (2.0, 3.0);;
let pts = [pt1; pt2; pt3];;
let edges = [(pt1, pt2); (pt2, pt3); (pt3, pt1)];;
let g = (pts, edges);;

printer g pt1 d;;
printer g pt2 d;;
printer g pt3 d;;
printer g (1.0,2.0) d;;
