module type S = sig
  type t

  val accept : t -> sw:Eio.Switch.t -> _ Stream_socket.t * Eio.Net.Sockaddr.stream
  val close : t -> unit
  val listening_addr : t -> Eio.Net.Sockaddr.stream
  val fd : t -> Fd.t
end

class type ['a] listening_socket = object
  method listening_socket : (module Eio.Net.Listening_socket.S with type t = 'a)
  method unix_listening_socket : (module S with type t = 'a)
  method close : 'a -> unit
  method fd : 'a -> Fd.t
  method resource_store : 'a Eio.Resource_store.t
end

type ('a, 'r) t =
  ('a *
   (< listening_socket : (module Eio.Net.Listening_socket.S with type t = 'a)
    ; unix_listening_socket : (module S with type t = 'a)
    ; close : 'a -> unit
    ; fd : 'a -> Fd.t
    ; resource_store : 'a Eio.Resource_store.t
    ; ..> as 'r))

type 'a t' = ('a, 'a listening_socket) t

type r = T : 'a t' -> r

val make : (module S with type t = 'a) -> 'a -> 'a t'

val close : _ t -> unit
val fd : _ t -> Fd.t
