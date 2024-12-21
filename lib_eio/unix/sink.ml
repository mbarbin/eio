type t =
  | T :
      ('a *
       < sink : (module Eio.Flow.SINK with type t = 'a)
       ; close : 'a -> unit
       ; fd : 'a -> Fd.t
       ; ..>)
      -> t [@@unboxed]

module Cast = struct
  let as_generic (T (a, ops)) = Eio.Flow.Sink (a, ops)
end

let close (T (a, ops)) = ops#close a
let fd (T (a, ops)) = ops#fd a
