

module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t

end

module type Algebra = functor (M : Monad) -> sig
  type a
  val handle : a M.t -> a
end
  

(****************************************************)
(* EXCEPTIONS                                       *)
(****************************************************)

(* Ex : Monad *)
module Ex = struct
  type ex = Bad of string
          | Worse of int
                   
  type 'a t = Raise of ex
            | Ok of 'a

  let return : 'a -> 'a t = fun x -> Ok x
  let bind (u : 'a t) (f : 'a -> 'b t) : 'b t =
    match u with
    | Raise e -> Raise e
    | Ok x -> f x
  
  let (let*) u f = bind u f
  (* OCaml way of specifying "do" notation, ie.
       let* x = t in u
     is the same as
       bind u (fun x -> t)
     or in Haskell,
       {do x <- t; u }
   *)
end

(* Ex_handler : Algebra(Ex) *)
module Ex_handler = struct
  type a = int
  let handle (u : a Ex.t) : a =
    match u with
    | Raise (Bad str) -> 0
    | Raise (Worse n) -> n
    | Ok v -> v
end

let div (x : int) (y : int) : int Ex.t =
  let open Ex in
  if y = 0 then
    Raise (Bad "divide by 0!")
  else
    Ok (x / y)

let arith (x : int) (y : int) : int =
  Ex_handler.handle
    (let open Ex in (* this brings Ex into scope *)
     let* u = return(x + y) in
     let* v = return(x - y) in
     div u v)

let _ = 
  let res = arith 10 10 in
  Format.printf "arith: %d\n" res




(****************************************************)
(* NONDETERMINISM                                   *)
(****************************************************)
  
(* ND : Monad *)          
module ND = struct
  type 'a t = 'a list

  let return : 'a -> 'a t = fun x -> [x]
  let bind (u : 'a t) (f : 'a -> 'b t) : 'b t =
    List.flatten (List.map f u)
  
  let (let*) (u : 'a t) (f : 'a -> 'b t) = bind u f
end

(* ND_handler : Algebra(ND) *)       
module ND_handler = struct
  type a = int

  let fail : a = 0
  let select : a -> a -> a = max
  
  let rec handle (u : a ND.t) : a =
    List.fold_right select u fail
end         

      
let maxsum xs ys : int =
  ND_handler.handle
    (let open ND in
     let* x = xs in
     let* y = ys in
     return (x + y))


let _ =
  let xs = [1;2;3;4] in
  let ys = [0;7;3;2] in
  let res = maxsum xs ys in
  Format.printf "maxsum: %d\n" res;

  
(****************************************************)
(* STATE                                            *)
(****************************************************)

(* State(S) : Monad *)  
module State (S : sig type t end) = struct
  type state = S.t
  type 'a t = state -> ('a * state)

  let return : 'a -> 'a t = fun x -> fun k -> (x,k)
  let bind (u : 'a t) (f : 'a -> 'b t) : 'b t =
    fun k ->
    let (x, k) = (u k) in
    (f x k)

  let get () : state t =
    (fun k -> (k,k))

  let set (k : state) : unit t =
    (fun _ -> ((),k))

  let run (u : 'a t) (init : state) : 'a =
    fst (u init)
  
  let (let*) = bind
end


(* This state algebra is very mysterious, and in this case unecessary,
 * but we specify it for illustrative purposes.
 * See:
 *  Plotkin and Power - Notions of computation determine monads *)

(* State_handler : Algebra(State(Int)) *)
module State_handler = struct
  type state = int
  type a = (state -> int)

  let lookup : (state -> a) -> a =
    fun f -> fun k -> f k k

  let update : a * state -> a =
    fun (u, k) -> (fun k' -> u k)

  let handle (u : a State(Int).t) : a =
    lookup (fun k -> update (u k))
end

let incr () : unit State(Int).t =
  let open State(Int) in
  let* x = get() in
  set (x + 1)

let do_stuff (x : int) : int =
  State_handler.handle
    (let open State(Int) in
     let* _ = incr() in
     let* _ = incr() in
     let* n = get() in
     let* _ = incr() in
     let* m = get() in
     let* _ = set (n + m) in
     let* u = get() in
     return (fun _ -> u))
    0

(* Here is the same example using 
 * run instead of the above handler *)
let do_stuff' (x : int) : int =
  let open State(Int) in
  run (let* _ = incr() in
       let* _ = incr() in
       let* n = get() in
       let* _ = incr() in
       let* m = get() in
       let* _ = set (n + m) in
       get())
    0
  
let _ = 
  let x = 10 in
  let res = do_stuff x in
  let res' = do_stuff' x in
  Format.printf "do_stuff: %d\n" res;
  Format.printf "do_stuff': %d\n" res'
  
