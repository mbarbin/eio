type t =
  | T :
      ('a *
       < shutdown : (module Eio.Flow.SHUTDOWN with type t = 'a)
       ; datagram_socket : (module Eio.Net.Datagram_socket.S with type t = 'a)
       ; close : 'a -> unit
       ; fd : 'a -> Fd.t
       ; resource_store : 'a Eio.Resource_store.t
       ; .. >)
      -> t [@@unboxed]

module Cast : sig
  val as_generic : t -> Eio.Net.Datagram_socket.t
end

val close : t -> unit
val fd : t -> Fd.t

module type S = sig
  include Eio.Net.Datagram_socket.S

  val fd : t -> Fd.t
end

module Pi : sig
  val make : (module S with type t = 'a) -> 'a -> t
end
