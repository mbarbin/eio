type t =
    T :
      ('a *
       < close : ('a -> unit) option; fd : ('a -> Fd.t) option;
         source : (module Eio.Flow.SOURCE with type t = 'a); .. >) -> 
      t [@@unboxed]
module Cast :
  sig
    val to_generic : t -> Eio.Flow.source
         end
    val of_generic : Eio.Flow.source -> t
