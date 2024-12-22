type t =
  | T :
      ('a *
       < sink : (module Eio.Flow.SINK with type t = 'a)
       ; close : ('a -> unit) option
       ; fd : ('a -> Fd.t) option
       ; resource_store : 'a Eio.Resource_store.t
       ; ..>)
      -> t [@@unboxed]

module Cast = struct
  let as_generic (T a) = Eio.Flow.Sink.T a
end

let of_generic (Eio.Flow.Sink.T (a, ops)) =
  T
    (a,
     object
       method sink = ops#sink
       method close = None
       method fd = None
       method resource_store = ops#resource_store
     end)

let fd (T (a, ops)) =
  match ops#fd with
  | None -> None
  | Some fd -> Some (fd a)

