module type S = sig
  type t

  val accept : t -> sw:Eio.Switch.t -> Stream_socket.t * Eio.Net.Sockaddr.stream
  val close : t -> unit
  val listening_addr : t -> Eio.Net.Sockaddr.stream
  val fd : t -> Fd.t
end

type t =
  | T :
      ('a *
       < listening_socket : (module Eio.Net.Listening_socket.S with type t = 'a)
       ; unix_listening_socket : (module S with type t = 'a)
       ; close : 'a -> unit
       ; fd : 'a -> Fd.t
       ; ..>)
      -> t [@@unboxed]

module Cast : sig
  val as_generic_listening_socket : t -> Eio.Net.Listening_socket.t
end

val close : t -> unit
val fd : t -> Fd.t

module Pi : sig
  val make : (module S with type t = 'a) -> 'a -> t
end
