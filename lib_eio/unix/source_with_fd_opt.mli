type t =
    T :
      ('a *
       < close : ('a -> unit) option
       ; source : (module Eio.Flow.SOURCE with type t = 'a)
       ; fd : ('a -> Fd.t) option
       ; resource_store : 'a Eio.Resource_store.t
       ; .. >)
      -> t [@@unboxed]

module Cast : sig
  val as_generic : t -> Eio.Flow.source
end

val of_generic : Eio.Flow.source -> t

val fd : t -> Fd.t option
