open Util ;;    
open CrawlerServices ;;
open Order ;;
open Pagerank ;;


(* RandomWalkRanker and QuantumRanker are for karma questions only *)
module MoogleRanker
  (* = InDegreeRanker (PageGraph) (PageScore) *)
  
  = RandomWalkRanker (PageGraph) (PageScore) (struct 
    let do_random_jumps = Some 0.20
    let num_steps = 1000
  end)
 
  (*  
   = QuantumRanker (PageGraph) (PageScore) (struct 
       let alpha = 0.01
       let num_steps = 1
       let debug = true
     end)
  *)

(* Dictionaries mapping words (strings) to sets of crawler links *)
module WordDict = Dict.Make(
  struct 
    type key = string
    type value = LinkSet.set
    let compare = string_compare
    let string_of_key = (fun s -> s)
    let string_of_value = LinkSet.string_of_set

    (* These functions are for testing purposes *)
    let gen_key () = ""
    let gen_key_gt x () = gen_key ()
    let gen_key_lt x () = gen_key ()
    let gen_key_random () = gen_key ()
    let gen_key_between x y () = None
    let gen_value () = LinkSet.empty
    let gen_pair () = (gen_key(),gen_value())
  end)

(* A query module that uses LinkSet and WordDict *)
module Q = Query.Query(
  struct
    module S = LinkSet
    module D = WordDict
  end)

let print s = 
  let _ = Printf.printf "%s\n" s in
  flush_all();;


(***********************************************************************)
(*    PART 1: CRAWLER                                                  *)
(***********************************************************************)

(* TODO: Build an index as follows:
 * 
 * Remove a link from the frontier (the set of links that have yet to
 * be visited), visit this link, add its outgoing links to the
 * frontier, and update the index so that all words on this page are
 * mapped to linksets containing this url.
 *
 * Keep crawling until we've
 * reached the maximum number of links (n) or the frontier is empty. *)

let rec sizeSet s start =
  match LinkSet.choose s with 
  | None -> start
  | Some (_, s') -> sizeSet s' (start + 1)
;;

let rec crawl (n:int) (frontier: LinkSet.set)
    (visited : LinkSet.set) (d:WordDict.dict) : WordDict.dict = 

  if sizeSet visited 0  = n then d 
  else 
    match LinkSet.choose frontier with
    (* if frontier is empty *)
    | None -> d 
    | Some (ele, f') ->
      (* if already visited, simply move to the rest of the set  *)
      if LinkSet.member visited ele then crawl n f' visited d
      (* if not, visit it  *)
      else 
        (* get page of the link *)
        match CrawlerServices.get_page ele with
        (* if page is empty, no link to visit, move on *)
        | None -> crawl n f' visited d
        | Some page -> 
          let new_visited = LinkSet.insert ele visited in
          (* add all links in page to the f' *)
          let new_f' = List.fold_left (fun f l -> LinkSet.insert l f) f' page.links in
          (* add words to dic *)
          match page.words with 
          (* if words list is empty, no adding, move on *)
          | [] -> crawl n f' visited d
          | wds ->
            let new_dict = List.fold_left (
              fun d w -> 
                let w = String.lowercase_ascii w in
                match WordDict.lookup d w with
                (* if there is no value for the word, create one *)
                | None -> WordDict.insert d w (LinkSet.singleton ele)
                (* if there are already some values, add the new one to the list *)
                | Some v -> WordDict.insert d w (LinkSet.insert ele v)
            ) d wds
            in crawl n new_f' new_visited new_dict
;;

let crawler () = 
  crawl num_pages_to_search (LinkSet.singleton initial_link) LinkSet.empty
    WordDict.empty
;;

(* Debugging note: if you set debug=true in moogle.ml, it will print out your
 * index after crawling. *)
