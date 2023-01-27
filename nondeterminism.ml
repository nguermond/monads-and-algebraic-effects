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
       | Print : string -> unit Effect.t


    let fail () = raise Fail
    let decide () = perform (Decide())
    let print str = perform (Print str)

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
                               M.op (continue k true)
                                 (continue k false)))
            | Print str -> (Some (fun k ->
                                prerr_endline str;
                                continue k ()))
            | _ -> None);
        }
  end

module IntList =
  struct
    type t = int list
    let id = []
    let op x y = x @ y
  end

module Boolean =
  struct
    type t = bool
    let id = false
    let op x y = (x || y)
  end

type tree = Leaf of int
          | Node of tree * tree

   
let rec search (key : int) (tt : tree) : bool =
  let open ND(Boolean) in
  let handle (tt : tree) : bool =
    match tt with
    | Leaf k -> (if k = key then true else fail())
    | Node (l,r) -> 
       if (decide()) then
         (print "left"; search key l)
       else
         (print "right"; search key r)
  in
  run handle tt


let rec findall (p : int -> bool) (tt : tree) : int list =
  let open ND(IntList) in
  let handle (tt : tree) : int list =
    match tt with
    | Leaf k -> (if (p k) then [k] else fail())
    | Node (l,r) ->
       if (decide()) then
         (print "left"; findall p l)
       else
         (print "right"; findall p r)
  in
  run handle tt
  
let _ =
  let tt = (Node (Node (Leaf 1,Leaf 2), Node (Leaf 3,Leaf 4))) in
  let found = (search 3 tt) in
  let solns = (findall (fun n -> n > 2) tt) in 
  Format.printf "Found: %b\n" found;
  Format.printf "Solns: %s\n" (String.concat " " (List.map string_of_int solns))
