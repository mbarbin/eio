module type S = sig
  include Flow.SINK
  include File_ro.S with type t := t

  val pwrite : t -> file_offset:Optint.Int63.t -> Cstruct.t list -> int
  val sync : t -> unit
  val truncate : t -> Optint.Int63.t -> unit
end

class type ['a] file_rw = object
  method read : (module File_ro.S with type t = 'a)
  method source : (module Flow.SOURCE with type t = 'a)
  method close : 'a -> unit
  method write : (module S with type t = 'a)
  method sink : (module Flow.SINK with type t = 'a)
  method resource_store : 'a Resource_store.t
end

type ('a, 'r) t =
  ('a *
   (< read : (module File_ro.S with type t = 'a)
    ; source : (module Flow.SOURCE with type t = 'a)
    ; close : 'a -> unit
    ; write : (module S with type t = 'a)
    ; sink : (module Flow.SINK with type t = 'a)
    ; resource_store : 'a Resource_store.t
    ; .. > as 'r))
(** A file opened for reading and writing. *)

type 'a t' = ('a, 'a file_rw) t

type r = T : 'a t' -> r

let make (type t) (module X : S with type t = t) (t : t) =
  let resource_store = Resource_store.create () in
  (t, object
     method source = (module X : Flow.SOURCE with type t = t)
     method read = (module X : File_ro.S with type t = t)
     method close = X.close
     method sink = (module X : Flow.SINK with type t = t)
     method write = (module X : S with type t = t)
     method resource_store = resource_store
   end)

let find_store (type a) ((t, ops) : (a, _) t) { Resource_store. key } =
  Resource_store.find ops#resource_store ~key
  |> Option.map (fun f -> f t)

let close (type a) (t : (a, _) t) = Closable.close t
