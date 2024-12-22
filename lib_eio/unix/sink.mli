type t =
    T :
      ('a *
       < close : 'a -> unit
       ; fd : 'a -> Fd.t
       ; sink : (module Eio.Flow.SINK with type t = 'a)
       ; resource_store : 'a Eio.Resource_store.t
       ; .. >) -> t [@@unboxed]

module Cast : sig
  val as_generic : t -> Eio.Flow.sink
end

val close : t -> unit
val fd : t -> Fd.t

