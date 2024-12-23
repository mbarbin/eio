module type S = sig
  include Flow.SHUTDOWN
  val send : t -> ?dst:Sockaddr.datagram -> Cstruct.t list -> unit
  val recv : t -> Cstruct.t -> Sockaddr.datagram * int
  val close : t -> unit
end

class type ['a] datagram_socket = object
  method shutdown : (module Flow.SHUTDOWN with type t = 'a)
  method datagram_socket : (module S with type t = 'a)
  method close : 'a -> unit
  method resource_store : 'a Resource_store.t
end

type ('a, 'r) t =
  ('a *
   (< shutdown : (module Flow.SHUTDOWN with type t = 'a)
    ; datagram_socket : (module S with type t = 'a)
    ; close : 'a -> unit
    ; resource_store : 'a Resource_store.t
    ; .. > as 'r))

type 'a t' = ('a, 'a datagram_socket) t

type r = T : 'a t' -> r

val make : (module S with type t = 'a) -> 'a -> 'a t'

val find_store : _ t -> 'b Resource_store.accessor -> 'b option

val close : _ t -> unit
