type t =
    T :
      ('a *
       < close : 'a -> unit; fd : 'a -> Fd.t;
         sink : (module Eio.Flow.SINK with type t = 'a); .. >) -> t [@@unboxed]

module Cast : sig
  val as_generic : t -> Eio.Flow.sink
end

val close : t -> unit
val fd : t -> Fd.t

