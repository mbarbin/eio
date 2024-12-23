module type S = sig
  include Flow.SHUTDOWN
  include Flow.SOURCE with type t := t
  include Flow.SINK with type t := t
  val close : t -> unit
end

class type ['a] stream_socket = object
  method shutdown : (module Flow.SHUTDOWN with type t = 'a)
  method source : (module Flow.SOURCE with type t = 'a)
  method sink : (module Flow.SINK with type t = 'a)
  method close : 'a -> unit
  method resource_store : 'a Resource_store.t
end

type ('a, 'r) t =
  ('a *
   (< shutdown : (module Flow.SHUTDOWN with type t = 'a)
    ; source : (module Flow.SOURCE with type t = 'a)
    ; sink : (module Flow.SINK with type t = 'a)
    ; close : 'a -> unit
    ; resource_store : 'a Resource_store.t
    ; .. > as 'r))

type 'a t' = ('a, 'a stream_socket) t

type r = T : 'a t' -> r

let make (type t) (module X : S with type t = t) (t : t) =
  let resource_store = Resource_store.create () in
  (t, object
     method close = X.close
     method shutdown = (module X : Flow.SHUTDOWN with type t = t)
     method source = (module X : Flow.SOURCE with type t = t)
     method sink = (module X : Flow.SINK with type t = t)
     method resource_store = resource_store
   end)

(* CR mbarbin: Share this method somewhere, it is used consistently in
   this design. *)
let find_store (type a) ((t, ops) : (a, _) t) { Resource_store. key } =
  Resource_store.find ops#resource_store ~key
  |> Option.map (fun f -> f t)

let close (type a) (t : (a, _) t) = Closable.close t
