type t =
  | T :
      ('a *
       < source : (module Eio.Flow.SOURCE with type t = 'a)
       ; close : 'a -> unit
       ; fd : 'a -> Fd.t
       ; ..>)
      -> t [@@unboxed]

module Cast = struct
  let as_generic (T a) = Eio.Flow.Source.T a
end

let close (T (a, ops)) = ops#close a
let fd (T (a, ops)) = ops#fd a
