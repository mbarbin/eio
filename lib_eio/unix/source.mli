class type ['a] source = object
  method close : 'a -> unit
  method fd : 'a -> Fd.t
  method source : (module Eio.Flow.SOURCE with type t = 'a)
  method resource_store : 'a Eio.Resource_store.t
end

type ('a, 'r) t =
  ('a *
   (< close : 'a -> unit
    ; fd : 'a -> Fd.t
    ; source : (module Eio.Flow.SOURCE with type t = 'a)
    ; resource_store : 'a Eio.Resource_store.t
    ; .. > as 'r))

type 'a t' = ('a, 'a source) t

type r = T : 'a t' -> r

(* CR mbarbin: There is no make here. How are the object created? Downcasting only? *)

val close : _ t -> unit
val fd : _ t -> Fd.t
