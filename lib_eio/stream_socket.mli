module type S = sig
  include Flow.SHUTDOWN
  include Flow.SOURCE with type t := t
  include Flow.SINK with type t := t
  val close : t -> unit
end

class type ['a] stream_socket = object
  method shutdown : (module Flow.SHUTDOWN with type t = 'a)
  method source : (module Flow.SOURCE with type t = 'a)
  method sink : (module Flow.SINK with type t = 'a)
  method close : 'a -> unit
  method resource_store : 'a Resource_store.t
end

type ('a, 'r) t =
  ('a *
   (< shutdown : (module Flow.SHUTDOWN with type t = 'a)
    ; source : (module Flow.SOURCE with type t = 'a)
    ; sink : (module Flow.SINK with type t = 'a)
    ; close : 'a -> unit
    ; resource_store : 'a Resource_store.t
    ; .. > as 'r))

type 'a t' = ('a, 'a stream_socket) t

type r = T : 'a t' -> r

val find_store : _ t -> 'b Resource_store.accessor -> 'b option

val close : _ t -> unit

val make : (module S with type t = 'a) -> 'a -> 'a t'
