open Effect
open Effect.Deep

module type Monoid =
  sig
    type t
    val id  : t
    val op : t -> t -> t
  end
           
module ND (M : Monoid) =
  struct
    (* Fail : unit -> empty Effect.t *)
    exception Fail
    
    type _ Effect.t +=
       | Decide : unit -> bool Effect.t


    let fail () = raise Fail
    let decide () = perform (Decide())

    let run (f : 'a -> 'b) (v : 'a) : M.t = 
      match_with f v
        { retc = (fun ret -> ret);
          exnc = (function
                    Fail -> M.id
                  | e -> raise e
                 );
          effc = (fun (type b) (eff: b Effect.t) ->
            match eff with
            | Decide() -> (Some (fun (k : (b, M.t) continuation) ->
                               (* continuations may only be used once unless cloned *)
                               let k' = (Multicont.Deep.clone_continuation k) in
                               M.op (continue k true) (continue k' false)))
            | _ -> None);
        }
  end


type tree = Leaf of int
          | Node of tree * tree


module Boolean =
  struct
    type t = bool
    let id = false
    let op x y = (x || y)
  end

let rec search (key : int) (tt : tree) : bool =
  let open ND(Boolean) in
  let handle (tt : tree) : bool =
    match tt with
    | Leaf k -> (if k = key then true else fail())
    | Node (l,r) -> 
       if (decide()) then
         (search key l)
       else
         (search key r)
  in
  run handle tt


module IntList =
  struct
    type t = int list
    let id = []
    let op x y = x @ y
  end

let rec findall (p : int -> bool) (tt : tree) : int list =
  let open ND(IntList) in
  let handle (tt : tree) : int list =
    match tt with
    | Leaf k -> (if (p k) then [k] else fail())
    | Node (l,r) ->
       if (decide()) then
         (findall p l)
       else
         (findall p r)
  in
  run handle tt


module Max =
  struct
    type t = int
    let id = 0
    let op x y = max x y
  end

(* find values k1 in tt1, k2 in tt2 such that (k1 + k2) is maximized *)
let maxsum (tt1 : tree) (tt2 : tree) : int =
  let open ND(Max) in
  let rec findmax (f : int -> int) (tt : tree) : int =
    match tt with
    | Leaf k -> (f k)
    | Node (l,r) ->
       if (decide()) then
         (findmax f l)
       else
         (findmax f r)
  in
  run (findmax (fun k -> (findmax (fun l -> k + l) tt2))) tt1


let _ =
  let tt = Node (Node (Leaf 3,Leaf 7), Node (Leaf 11,Leaf 4)) in
  let tt' = Node (Node (Leaf 5,Node(Leaf 7,Leaf 0)), Node(Node(Leaf 3,Leaf 4),Leaf 6)) in
  let found = (search 3 tt) in
  let solns = (findall (fun n -> n > 4) tt) in
  let msum = (maxsum tt tt') in
  Format.printf "Found: %b\n" found;
  Format.printf "Solns: %s\n" (String.concat " " (List.map string_of_int solns));
  Format.printf "Max sum: %d\n" msum
