open Eio.Std

module type STREAM_SOCKET = sig
  include Eio.Net.STREAM_SOCKET

  val send_msg : t -> fds:Fd.t list -> Cstruct.t list -> int
  val recv_msg_with_fds : t -> sw:Switch.t -> max_fds:int -> Cstruct.t list -> int * Fd.t list

  val fd : t -> Fd.t
end

module type FLOW = sig
  include Eio.File.WRITE
  include STREAM_SOCKET with type t := t
end

type flow =
  | Flow :
      ('a *
       < shutdown : (module Eio.Flow.SHUTDOWN with type t = 'a)
       ; source : (module Eio.Flow.SOURCE with type t = 'a)
       ; sink : (module Eio.Flow.SINK with type t = 'a)
       ; close : 'a -> unit
       ; read : (module Eio.File.READ with type t = 'a)
       ; write : (module Eio.File.WRITE with type t = 'a)
       ; fd : 'a -> Fd.t
       ; stream_socket : (module STREAM_SOCKET with type t = 'a)
       ; ..>)
      -> flow [@@unboxed]

val flow : (module FLOW with type t = 'a) -> 'a -> flow

module type DATAGRAM_SOCKET = sig
  include Eio.Net.DATAGRAM_SOCKET

  val fd : t -> Fd.t
end

type datagram =
  | Datagram :
      ('a *
       < shutdown : (module Eio.Flow.SHUTDOWN with type t = 'a)
       ; datagram_socket : (module Eio.Net.DATAGRAM_SOCKET with type t = 'a)
       ; close : 'a -> unit
       ; fd : 'a -> Fd.t
       ; .. >)
      -> datagram [@@unboxed]

val datagram : (module DATAGRAM_SOCKET with type t = 'a) -> 'a -> datagram

module type LISTENING_SOCKET = sig
  include Eio.Net.LISTENING_SOCKET

  val fd : t -> Fd.t
end

type listening_socket =
  | Listening_socket :
      ('a *
       < listening_socket : (module Eio.Net.LISTENING_SOCKET with type t = 'a)
       ; close : 'a -> unit
       ; fd : 'a -> Fd.t
       ; ..>)
      -> listening_socket [@@unboxed]

val listening_socket :
  (module LISTENING_SOCKET with type t = 'a) -> 'a -> listening_socket
