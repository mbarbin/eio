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

let flow (type a) (module X : FLOW with type t = a) (t : a) =
  Flow
    (t, object
       method shutdown = (module X : Eio.Flow.SHUTDOWN with type t = a)
       method source = (module X : Eio.Flow.SOURCE with type t = a)
       method sink = (module X : Eio.Flow.SINK with type t = a)
       method close = X.close
       method read = (module X : Eio.File.READ with type t = a)
       method write = (module X : Eio.File.WRITE with type t = a)
       method fd = X.fd
       method stream_socket = (module X : STREAM_SOCKET with type t = a)
     end)

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

let datagram (type a) (module X : DATAGRAM_SOCKET with type t = a) (t : a) =
  Datagram
    (t, object
       method shutdown = (module X : Eio.Flow.SHUTDOWN with type t = a)
       method datagram_socket = (module X : Eio.Net.DATAGRAM_SOCKET with type t = a)
       method close = X.close
       method fd = X.fd
     end)

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

let listening_socket (type a) (module X : LISTENING_SOCKET with type t = a) (t : a) =
  Listening_socket
    (t, object
        method listening_socket = (module X : Eio.Net.LISTENING_SOCKET with type t = a)
        method close = X.close
        method fd = X.fd
     end)

