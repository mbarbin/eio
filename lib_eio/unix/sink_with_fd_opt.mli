type t =
    T :
      ('a *
       < close : ('a -> unit) option
       ; sink : (module Eio.Flow.SINK with type t = 'a)
       ; fd : ('a -> Fd.t) option
       ; .. >)
      -> t [@@unboxed]

module Cast :
sig
  val as_generic : t -> Eio.Flow.sink
end

val of_generic : Eio.Flow.sink -> t

val fd : t -> Fd.t option
