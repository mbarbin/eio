type t =
    T :
      ('a *
       < close : ('a -> unit) option; fd : ('a -> Fd.t) option;
         sink : (module Eio.Flow.SINK with type t = 'a); .. >) -> t [@@unboxed]

module Cast :
sig
  val as_generic : t -> Eio.Flow.sink
end

val of_generic : Eio.Flow.sink -> t
