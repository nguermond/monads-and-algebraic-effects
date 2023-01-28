
(****************************************************)
(* EXCEPTIONS                                       *)
(****************************************************)


                 
module Ex = struct
  type ex = Bad of string
          | Worse of int
                   

  effect Raise : ex -> empty
  
  let div (x : int) (y : int) : int =
    if y = 0 then
      Raise (Bad "divide by 0!")
    else
      (x / y)

  let arith x y : int =
    try
      let u = x + y in
      let v = x - y in
      div u v
    with
      (* Raise : ex -> (empty -> int) -> int *)  
      Raise(e; k) -> 0
  in
  arith 10 10
end



(****************************************************)
(* STATE                                            *)
(****************************************************)


module State = struct
  type state = int

  effect Set : state -> unit  
  effect Get : unit -> state  
  
  let incr : unit -> unit =
    let n = Get () in
    Set (n + 1)

  let result : state -> state =
    try
      incr ();
      incr ();
      Get()
    with
      (* get : unit -> (state -> (state -> 'r)) -> (state -> 'r) *)
    | Get ((); k) -> (fun s -> k s s)
      (* set : state -> (unit -> (state -> 'r)) -> (state -> 'r) *)
    | Set (s;  k) -> (fun _ -> k () s)
  in
  result 0
end




(****************************************************)
(* NONDETERMINISM                                   *)
(****************************************************)

module type Monoid = sig
    type t
    val id  : t
    val op : t -> t -> t
  end

module ND (M : Monoid) = struct
    effect Fail : unit -> empty
    effect Decide : unit -> bool

    let run (f : 'a -> 'b) (v : 'a) : M.t = 
      try
        (f v)
      with
      (* fail : unit -> (empty -> M.t) -> M.t *)
      | Fail ((); k)   -> M.id
      (* decide : unit -> (bool -> M.t) -> M.t *)
      | Decide ((); k) -> M.op (continue k true) (continue k false)
  end



module IntList = struct
  type t = int list
  let id = []
  let op x y = x @ y
end

module Boolean = struct
  type t = bool
  let id = false
  let op x y = (x || y)
end

module Max = struct
  type t = int
  let id = 0
  let op x y = max x y
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
         (search key l)
       else
         (search key r)
  in
  run handle tt


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
  run (findmax (fun k -> run (findmax (fun l -> k + l)) tt2)) tt1
  
