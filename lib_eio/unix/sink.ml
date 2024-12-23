class type ['a] sink = object
  method close : 'a -> unit
  method fd : 'a -> Fd.t
  method sink : (module Eio.Flow.SINK with type t = 'a)
  method resource_store : 'a Eio.Resource_store.t
end

type ('a, 'r) t =
  ('a *
   (< close : 'a -> unit
    ; fd : 'a -> Fd.t
    ; sink : (module Eio.Flow.SINK with type t = 'a)
    ; resource_store : 'a Eio.Resource_store.t
    ; .. > as 'r))

type 'a t' = ('a, 'a sink) t

type r = T : 'a t' -> r [@@unboxed]

let close (type a) ((a, ops) : (a, _) t) = ops#close a
let fd (type a) ((a, ops) : (a, _) t) = ops#fd a

module Cast = struct
  let as_closable_generic (T (a, ops)) =
    Eio.Flow.Closable_sink.T
      (a, (ops :> _ Eio.Flow.Closable_sink.closable_sink))

  let as_generic (T (a, ops)) =
    Eio.Flow.Sink.T
      (a, (ops :> _ Eio.Flow.Sink.sink))
end
