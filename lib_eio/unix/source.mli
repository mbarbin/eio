type t =
  | T :
      ('a *
       < close : 'a -> unit; fd : 'a -> Fd.t
       ; source : (module Eio.Flow.SOURCE with type t = 'a)
       ; .. >)
      -> t [@@unboxed]

module Cast : sig
  val as_generic : t -> Eio.Flow.source
end

val close : t -> unit
val fd : t -> Fd.t
