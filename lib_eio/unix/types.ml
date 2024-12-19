module Source = struct
  type t =
    | T :
        ('a *
         < source : (module Eio.Flow.SOURCE with type t = 'a)
         ; close : 'a -> unit
         ; fd : 'a -> Fd.t
         ; ..>)
        -> t [@@unboxed]

  let to_generic (T (a, ops)) = Eio.Flow.Source (a, ops)
  let close (T (a, ops)) = ops#close a
  let fd (T (a, ops)) = ops#fd a
end

module Sink = struct
  type t =
    | T :
        ('a *
         < sink : (module Eio.Flow.SINK with type t = 'a)
         ; close : 'a -> unit
         ; fd : 'a -> Fd.t
         ; ..>)
        -> t [@@unboxed]

  let to_generic (T (a, ops)) = Eio.Flow.Sink (a, ops)
  let close (T (a, ops)) = ops#close a
  let fd (T (a, ops)) = ops#fd a
end
