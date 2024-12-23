module type S = sig
  include Flow.SOURCE

  val pread : t -> file_offset:Optint.Int63.t -> Cstruct.t list -> int
  val stat : t -> Stat.t
  val seek : t -> Optint.Int63.t -> [`Set | `Cur | `End] -> Optint.Int63.t
  val close : t -> unit
end

class type ['a] file_ro = object
  method read : (module S with type t = 'a)
  method source : (module Flow.SOURCE with type t = 'a)
  method close : 'a -> unit
  method resource_store : 'a Resource_store.t
end

type ('a, 'r) t =
  ('a *
   (< read : (module S with type t = 'a)
    ; source : (module Flow.SOURCE with type t = 'a)
    ; close : 'a -> unit
    ; resource_store : 'a Resource_store.t
    ; ..> as 'r))

type 'a t' = ('a, 'a file_ro) t

type r = T : 'a t' -> r [@@unboxed]

let make (type t) (module X : S with type t = t) (t : t) =
  let resource_store = Resource_store.create () in
  (t, object
     method source = (module X : Flow.SOURCE with type t = t)
     method read = (module X : S with type t = t)
     method close = X.close
     method resource_store = resource_store
   end)

let find_store (type a) ((t, ops) : (a, _) t) { Resource_store. key } =
  Resource_store.find ops#resource_store ~key
  |> Option.map (fun f -> f t)

let close (type a) (t : (a, _) t) = Closable.close t
