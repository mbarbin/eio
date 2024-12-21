type t =
  | T :
      ('a *
       < listening_socket : (module Eio.Net.Listening_socket.S with type t = 'a)
       ; close : 'a -> unit
       ; fd : 'a -> Fd.t
       ; ..>)
      -> t [@@unboxed]

module Cast : sig
  val as_generic_listening_socket : t -> Eio.Net.Listening_socket.t
end

val close : t -> unit
val fd : t -> Fd.t

module type S = sig
  include Eio.Net.Listening_socket.S

  val fd : t -> Fd.t
end

module Pi : sig
  val make : (module S with type t = 'a) -> 'a -> t
end
