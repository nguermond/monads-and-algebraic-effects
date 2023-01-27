open Effect
open Effect.Deep


type t = int
       
type _ Effect.t +=
   | Get : unit -> t Effect.t
   | Set : t -> unit Effect.t


let get () : int = perform (Get ())
let set x : unit = perform (Set x)

let incr () : unit =
  let n = get () in
  set (n + 1)
  
let do_stuff () : t =
  let n = get () in
  incr ();
  incr ();
  let m = get() in
  set (n + m);
  get ()
  
let result : t -> t =
  match_with do_stuff ()
    { effc = (fun (type c) (eff : c Effect.t) ->
        match eff with
        | Get () -> Some (fun (k : (c,t -> 'r) continuation) ->
                        (fun (s : t) -> continue k s s))
                  
        | Set s -> Some (fun (k : (c,t -> 'r) continuation) ->
                       (fun _ -> continue k () s))
                 
        | _ -> None
      );
      exnc = (fun e -> (fun s -> raise e));
      retc = (fun res -> (fun s -> res))
    }
  
let _ = Format.printf "Final value: %d\n" (result 10)
