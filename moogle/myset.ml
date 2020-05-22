(* Definitions for sets. *)

exception TODO

(* An interface for set modules *)
module type SET = 
sig
  type elt  (* type of elements in the set *)
  type set  (* abstract type for the set *)

  val empty : set

  val is_empty : set -> bool

  val insert : elt -> set -> set

  (* same as insert x empty *)
  val singleton : elt -> set

  val union : set -> set -> set
  val intersect : set -> set -> set

  (* remove an element from the set -- if the
   * element isn't present, does nothing. *)
  val remove : elt -> set -> set

  (* returns true iff the element is in the set *)
  val member : set -> elt -> bool

  (* chooses some member from the set, removes it 
   * and returns that element plus the new set.  
   * If the set is empty, returns None. *)
  val choose : set -> (elt * set) option

  (* fold a function across the elements of the set
   * in some unspecified order. *)
  val fold : (elt -> 'a -> 'a) -> 'a -> set -> 'a

  (* functions to convert our types to a string. useful for debugging. *)
  val string_of_set : set -> string
  val string_of_elt : elt -> string

  (* runs our tests. See "Testing" in the project description web page *)
  val run_tests : unit -> unit
end



(* parameter to Set modules -- we must pass in some 
 * type for the elements of a set, a comparison
 * function, and a way to stringify it.
 *)
module type COMPARABLE = 
sig
  type t
  val compare : t -> t -> Order.order
  val string_of_t : t -> string

  (* The functions below are used for testing.
     See "Testing" in the project description web page *)

  (* Generate a value of type t. The same t is always returned *)
  val gen : unit -> t

  (* Generate a random value of type t. *)
  val gen_random : unit -> t

  (* Generate a t greater than the argument. *)
  val gen_gt : t -> unit -> t

  (* Generate a t less than the argument. *)
  val gen_lt : t -> unit -> t

  (* Generate a t between the two arguments. Return None if no such
   * t exists. *)
  val gen_between : t -> t -> unit -> t option
end



(* An example implementation of our COMPARABLE signature. Use this
 * struct for testing. *)
module IntComparable : COMPARABLE =
struct
  open Order
  type t = int
  let compare x y = if x < y then Less else if x > y then Greater else Eq
  let string_of_t = string_of_int
  let gen () = 0
  let gen_random =
    let _ = Random.self_init () in
    (fun () -> Random.int 10000)
  let gen_gt x () = x + 1
  let gen_lt x () = x - 1
  let gen_between x y () = 
    let (lower, higher) = (min x y, max x y) in
    if higher - lower < 2 then None else Some (higher - 1)
end



(* A simple, list-based implementation of sets. *)
module ListSet(C: COMPARABLE) : (SET with type elt = C.t) = 
struct
  open Order
  type elt = C.t 
  type set = elt list

  (* INVARIANT: sorted, no duplicates *)
  let empty = []
  let is_empty xs = 
    match xs with 
      | [] -> true
      | _ -> false
  let singleton x = [x]
  let rec insert x xs = 
    match xs with 
      | [] -> [x]
      | y::ys -> (match C.compare x y with 
          | Greater -> y::(insert x ys)
          | Eq -> xs
          | Less -> x::xs)

  let union xs ys = List.fold_right insert xs ys
  let rec remove y xs = 
    match xs with 
      | [] -> []
      | x::xs1 -> (match C.compare y x with 
          | Eq -> xs1
          | Less -> xs
          | Greater -> x::(remove y xs1))

  let rec intersect xs ys = 
    match xs, ys with 
      | [], _ -> []
      | _, [] -> []
      | xh::xt, yh::yt -> (match C.compare xh yh with 
          | Eq -> xh::(intersect xt yt)
          | Less -> intersect xt ys
          | Greater -> intersect xs yt)

  let rec member xs x = 
    match xs with 
      | [] -> false
      | y::ys -> (match C.compare x y with
          | Eq -> true
          | Greater -> member ys x
          | Less -> false)

  let choose xs = 
    match xs with 
      | [] -> None
      | x::rest -> Some (x,rest)
  let fold f e = List.fold_left (fun a x -> f x a) e 
    
  let string_of_elt = C.string_of_t
  let string_of_set (s: set) : string = 
    let f = (fun y e -> y ^ "; " ^ C.string_of_t e) in
    "set([" ^ (List.fold_left f "" s) ^ "])"


  (****************************************************************)
  (* Tests for our ListSet functor                                *)
  (* These are just examples of tests, your tests should be a lot *)
  (* more thorough than these.                                    *)
  (****************************************************************)

  (* adds a list of (key,value) pairs in left-to-right order *)
  let insert_list (d: set) (lst: elt list) : set = 
    List.fold_left (fun r k -> insert k r) d lst

  let rec generate_random_list (size: int) : elt list =
    if size <= 0 then []
    else (C.gen_random()) :: (generate_random_list (size - 1))

  let test_insert () =
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    List.iter (fun k -> assert(member s1 k)) elts ;
    ()

  let test_remove () =
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    let s2 = List.fold_right (fun k r -> remove k r) elts s1 in
    List.iter (fun k -> assert(not (member s2 k))) elts ;
    ()

  let test_union () =
    ()

  let test_intersect () =
    ()

  let test_member () =
    ()

  let test_choose () =
    ()

  let test_fold () =
    ()

  let test_is_empty () =
    ()

  let test_singleton () =
    ()

  let run_tests () = 
    test_insert () ;
    test_remove () ;
    test_union () ;
    test_intersect () ;
    test_member () ;
    test_choose () ;
    test_fold () ;
    test_is_empty () ;
    test_singleton () ;
    ()
end


(******************************************************************)
(* DictSet: a functor that creates a SET by calling our           *)
(* Dict.Make functor                                              *)
(******************************************************************)

module DictSet(C : COMPARABLE) : (SET with type elt = C.t) = 
struct
  module D = Dict.Make(struct
    type key = C.t
		type value = unit
		let compare = C.compare
    let string_of_key = C.string_of_t
    let string_of_value = (fun () -> "")

    let gen_key () = C.gen ()
    let gen_key_gt x () = gen_key ()
    let gen_key_lt x () = gen_key ()
    let gen_key_random () = gen_key ()
    let gen_key_between x y () = C.gen_between x y ()
		let gen_value = (fun () -> ())
    let gen_pair () = (gen_key(),gen_value())
  end)
  
  type elt = D.key
  type set = D.dict
  let empty = D.empty

  (* implement the rest of the functions in the signature! *)
  let is_empty d = 
    match D.choose d with
      | None -> true
      | Some _ -> false

  let insert k d = D.insert d k ()

  let singleton k = D.insert D.empty k ()

  let union d d' = 
    if d = D.empty then d'
    else if d' = D.empty then d
    else D.fold (fun k v d -> D.insert d' k v) D.empty d

  let intersect d d' = 
    let aux = fun k v d -> 
                        if D.member d' k then D.insert d k v
                        else d
    in
    D.fold aux D.empty d

  let remove k d = D.remove d k

  let member d k = D.member d k

  let choose d = 
    match D.choose d with
      | None -> None
      | Some (k, v, set) -> Some (k, set)
  
  let fold f b = D.fold (fun k v d -> f k d) b

  let string_of_elt = D.string_of_key
  let string_of_set s = D.string_of_dict s

  (****************************************************************)
  (* Tests for our DictSet functor                                *)
  (* Use the tests from the ListSet functor to see how you should *)
  (* write tests. However, you must write a lot more              *)
  (* comprehensive tests to test ALL your functions.              *)
  (****************************************************************)

  (* add your test functions to run_tests *)

  let test_is_empty () = 
    assert (is_empty D.empty);

    let randomK = C.gen_random() in
    assert(not (is_empty (D.insert D.empty randomK ())));
    ()
  
  let rec sizeSet s start =
    match D.choose s with 
    | None -> start
    | Some (_, _, d) -> sizeSet d (start + 1)
  
  let test_insert () = 

    let randomK = C.gen_random() in
    let s1 = D.empty in

    (* check size of s1 *)
    assert(sizeSet s1 0 = 0);

    (* make sure randomK is not in s1 *)
    assert(is_empty s1);
    assert(not (member s1 randomK));

    (* insert randomK to s1 *)
    let s = D.insert s1 randomK () in
    (* test if randomK is in s1 now *)
    assert(member s randomK);

    (* check size of s after adding randomK *)
    assert(sizeSet s 0 = 1);

    (* test if we add randomK again to s, 
    it won't change the size -> no duplicate *)
    let s' = D.insert s randomK () in
    assert(sizeSet s' 0 = 1);
    assert(member s' randomK);
    ()

  let test_singleton () =
    let randomK = C.gen_random() in

    let s1 = singleton randomK in
    assert(member s1 randomK);
    assert(sizeSet s1 0 = 1);
    ()

  let test_union () = 
    let k1 = C.gen_random() in
    let k2 = C.gen_lt k1 () in

    let s1 = singleton k1 in 
    let s2 = singleton k2 in 
    (* add k1 to s2 -> s12 = {k1, k2} *)
    let s12 = D.insert s2 k1 () in

    (* make sure s12 has 2 elements k1 and k2 *)
    assert(sizeSet s12 0 = 2);

    assert(union s1 s2 = s12);

    (* union empty *)
    assert(union D.empty D.empty = D.empty);
    assert(union D.empty s12 = s12);
    assert(union s12 D.empty = s12);
    ()
  
  let test_intersect () =
    let k1 = C.gen_random() in
    let k2 = C.gen_gt k1 () in

    let s1 = singleton k1 in 
    let s2 = singleton k2 in 

    (* s1 intersect s2 = 0 (size) or empty *)
    assert(intersect s1 s2 = D.empty);
    assert(sizeSet (intersect s1 s2) 0 = 0);

    let s12 = D.insert s2 k1 () in
    assert(intersect s12 s1 = s1);
    assert(intersect s12 s2 = s2);
    assert(member (intersect s12 s1) k1);
    assert(member (intersect s12 s2) k2);

    (* intersect empty set *)
    assert(intersect D.empty D.empty = D.empty);
    assert(intersect D.empty s12 = D.empty);
    assert(intersect s12 D.empty = D.empty);
    ()
  
  let test_remove () =
    let k = C.gen_random() in

    let s = singleton k in
    assert(sizeSet s 0 = 1);
    assert(member s k);
    assert(remove k s = D.empty);

    let s' = D.insert s (C.gen_gt k ()) () in
    assert(sizeSet s' 0 = 2);
    assert(sizeSet (remove k s') 0 = 1);
    assert(not (member (remove k s') k));


    (* remove from empty *)
    assert(remove (C.gen()) D.empty = D.empty);
    ()
  
  let test_member () =
    let k = C.gen_random() in

    let s = singleton k in

    assert(member s k);
    assert(not (member D.empty k));
    ()
    
  let test_choose () =
    let k = C.gen_random() in

    let s = singleton k in

    (* choose k from s *)
    match choose s with
    | None -> assert (false);
    | Some (k', d) -> assert(k' = k); assert(d = D.empty);

    let s' = D.insert s (C.gen_gt k ()) () in 
    assert(sizeSet s' 0 = 2);

    match choose s' with 
    | None -> assert (false);
    | Some (k', d) -> 
      (* chosen k' is not in d *)  
      assert(not (member d k')); 
      (* size of d = s' - 1 *)
      assert(sizeSet d 0 = (sizeSet s' 0) - 1);
    ()

  let test_fold () =
    
    let k = C.gen_random() in
    let k1 = C.gen_gt k () in
    let k2 = C.gen_gt k1 () in

    let s = singleton k in
    let s = D.insert s k1 () in
    let s = D.insert s k2 () in
    assert(sizeSet s 0 = 3);

    let size_s = fold (fun x y -> y + 1) 0 s in
    assert(size_s = sizeSet s 0);
    ()

  let run_tests () = 
    test_is_empty();
    test_insert();
    test_singleton();
    test_union();
    test_intersect();
    test_remove();
    test_member();
    test_choose();
    test_fold();
    ()
end

(******************************************************************)
(* Run our tests.                                                 *)
(******************************************************************)

(* Create a set of ints using our ListSet functor. *)
(* module IntListSet = ListSet(IntComparable) ;;
IntListSet.run_tests();; *)

(* Create a set of ints using our DictSet functor
 * 
 * Uncomment out the lines below when you are ready to test your
 * 2-3 dict set implementation *)

module IntDictSet = DictSet(IntComparable) ;;
IntDictSet.run_tests();;


(******************************************************************)
(* Make: a functor that creates a SET by calling our              *)
(* ListSet or DictSet functors                                    *)
(******************************************************************)
module Make(C : COMPARABLE) : (SET with type elt = C.t) = 
  (* Change this line to use our dictionary implementation when your are 
   * finished. *)
  (* ListSet (C) *)
  DictSet (C)

