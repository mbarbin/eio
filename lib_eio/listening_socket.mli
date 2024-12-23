module type S = sig
  type t

  val accept : t -> sw:Switch.t -> Stream_socket.r * Sockaddr.stream
  val close : t -> unit
  val listening_addr : t -> Sockaddr.stream
end

class type ['a] listening_socket = object
  method listening_socket : (module S with type t = 'a)
  method close : 'a -> unit
  method resource_store : 'a Resource_store.t
end

 type ('a, 'r) t =
   ('a *
    (< listening_socket : (module S with type t = 'a)
     ; close : 'a -> unit
     ; resource_store : 'a Resource_store.t
     ; ..> as 'r))

type 'a t' = ('a, 'a listening_socket) t

type r = T : 'a t' -> r

val make : (module S with type t = 'a) -> 'a -> 'a t'

val find_store : _ t -> 'b Resource_store.accessor -> 'b option

val close : _ t -> unit


