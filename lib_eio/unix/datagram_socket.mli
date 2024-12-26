module type S = sig
  include Eio.Net.Datagram_socket.S

  val fd : t -> Fd.t
end

class type ['a] datagram_socket = object
  method shutdown : (module Eio.Flow.SHUTDOWN with type t = 'a)
  method datagram_socket : (module Eio.Net.Datagram_socket.S with type t = 'a)
  method close : 'a -> unit
  method fd : 'a -> Fd.t
  method resource_store : 'a Eio.Resource_store.t
end

type ('a, 'r) t =
  ('a *
   (< shutdown : (module Eio.Flow.SHUTDOWN with type t = 'a)
    ; datagram_socket : (module Eio.Net.Datagram_socket.S with type t = 'a)
    ; close : 'a -> unit
    ; fd : 'a -> Fd.t
    ; resource_store : 'a Eio.Resource_store.t
    ; .. > as 'r))

type 'a t' = ('a, 'a datagram_socket) t

type r = T : 'a t' -> r [@@unboxed]

val make : (module S with type t = 'a) -> 'a -> 'a t'

val close : _ t -> unit
val fd : _ t -> Fd.t

module Cast : sig
  val as_generic : r -> Eio.Net.Datagram_socket.r
end
