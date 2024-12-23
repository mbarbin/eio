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

type r = T : 'a t' -> r

val close : _ t -> unit
val fd : _ t -> Fd.t

