type t =
  | T :
      ('a *
       < sink : (module Eio.Flow.SINK with type t = 'a)
       ; close : ('a -> unit) option
       ; fd : ('a -> Fd.t) option
       ; ..>)
      -> t [@@unboxed]

module Cast = struct
  let to_generic (T (a, ops)) = Eio.Flow.Sink (a, ops)
end

let of_generic (Eio.Flow.Sink (a, ops)) =
  T
    (a,
     object
       method sink = ops#sink
       method close = None
       method fd = None
     end)
