open Eio.Std

module Stream_socket = Stream_socket
module Listening_socket = Listening_socket
module Datagram_socket = Datagram_socket

module Ipaddr = struct
  let to_unix : _ Eio.Net.Ipaddr.t -> Unix.inet_addr = Obj.magic
  let of_unix : Unix.inet_addr -> _ Eio.Net.Ipaddr.t = Obj.magic
end

let sockaddr_to_unix = function
  | `Unix path -> Unix.ADDR_UNIX path
  | `Tcp (host, port) | `Udp (host, port) ->
    let host = Ipaddr.to_unix host in
    Unix.ADDR_INET (host, port)

let sockaddr_of_unix_stream = function
  | Unix.ADDR_UNIX path -> `Unix path
  | Unix.ADDR_INET (host, port) ->
    let host = Ipaddr.of_unix host in
    `Tcp (host, port)

let sockaddr_of_unix_datagram = function
  | Unix.ADDR_UNIX path -> `Unix path
  | Unix.ADDR_INET (host, port) ->
    let host = Ipaddr.of_unix host in
    `Udp (host, port)

let send_msg (Stream_socket.T (t, ops)) ?(fds=[]) bufs =
  let module X = (val ops#stream_socket) in
  let rec aux ~fds bufs =
    let sent = X.send_msg t ~fds bufs in
    match Cstruct.shiftv bufs sent with
    | [] -> ()
    | bufs -> aux bufs ~fds:[]
  in
  aux ~fds bufs

let recv_msg_with_fds (Stream_socket.T (t, ops)) ~sw ~max_fds bufs =
  let module X = (val ops#stream_socket) in
  X.recv_msg_with_fds t ~sw ~max_fds bufs

let getnameinfo (sockaddr : Eio.Net.Sockaddr.t) =
  let options =
    match sockaddr with
    | `Unix _ | `Tcp _ -> []
    | `Udp _ -> [Unix.NI_DGRAM]
  in
  let sockaddr = sockaddr_to_unix sockaddr in
  Thread_pool.run_in_systhread ~label:"getnameinfo" (fun () ->
    let Unix.{ni_hostname; ni_service} = Unix.getnameinfo sockaddr options in
    (ni_hostname, ni_service))

module type S = sig
  type t

  val listen :
    t -> reuse_addr:bool -> reuse_port:bool -> backlog:int -> sw:Switch.t ->
    Eio.Net.Sockaddr.stream -> Listening_socket.t

  val connect : t -> sw:Switch.t -> Eio.Net.Sockaddr.stream -> Stream_socket.t

  val datagram_socket :
    t
    -> reuse_addr:bool
    -> reuse_port:bool
    -> sw:Switch.t
    -> [Eio.Net.Sockaddr.datagram | `UdpV4 | `UdpV6]
    -> Datagram_socket.t

  val getaddrinfo : t -> service:string -> string -> Eio.Net.Sockaddr.t list
  val getnameinfo : t -> Eio.Net.Sockaddr.t -> (string * string)
end

type 'r t =
  | Network :
      ('a *
       < network : (module Eio.Net.NETWORK with type t = 'a)
       ; network_unix : (module S with type t = 'a)
       ; ..> as 'r)
      -> 'r t [@@unboxed]

(* CR mbarbin: This is temporary code that I use to be able to compile
   some code that currently does not make use of [Eio_unix] specific
   functionality. I would like to spend more time thinking about this
   once I know more. *)
module To_generic (X : S) : Eio.Net.NETWORK with type t = X.t = struct
      include X

      let listen t ~reuse_addr ~reuse_port ~backlog ~sw stream =
        let listening_socket = X.listen t ~reuse_addr ~reuse_port ~backlog ~sw stream in
        Listening_socket.Cast.as_generic listening_socket

      let connect t ~sw stream =
        let stream_socket = X.connect t ~sw stream in
        Stream_socket.Cast.as_generic stream_socket

      let datagram_socket t ~reuse_addr ~reuse_port ~sw addr =
        let datagram_socket = X.datagram_socket t ~reuse_addr ~reuse_port ~sw addr in
        Datagram_socket.Cast.as_generic datagram_socket
    end

module Cast = struct
  let as_generic (Network t) = Eio.Net.Network t
end

let accept ~sw (Listening_socket.T (t, ops)) =
  let module X = (val ops#unix_listening_socket) in
  X.accept t ~sw

let listen (type a) ?(reuse_addr=false) ?(reuse_port=false) ~backlog ~sw (t : a t) =
  let (Network (t, ops)) = t in
  let module X = (val ops#network_unix) in
  X.listen t ~reuse_addr ~reuse_port ~backlog ~sw

let connect (type a) ~sw (t : a t) addr =
  let (Network (t, ops)) = t in
  let module X = (val ops#network_unix) in
  try X.connect t ~sw addr
  with Eio.Exn.Io _ as ex ->
    let bt = Printexc.get_raw_backtrace () in
    Eio.Exn.reraise_with_context ex bt "connecting to %a" Eio.Net.Sockaddr.pp addr

module Pi = struct
  let make (type a) (module X : S with type t = a) (t : a) =
    let module G = To_generic (X) in
    Network (t, object
      method network = (module G : Eio.Net.NETWORK with type t = a)
      method network_unix = (module X : S with type t = a)
    end)
end

[@@@alert "-unstable"]

type _ Effect.t +=
  | Import_socket_stream : Switch.t * bool * Unix.file_descr -> Stream_socket.t Effect.t
  | Import_socket_listening : Switch.t * bool * Unix.file_descr -> Listening_socket.t Effect.t
  | Import_socket_datagram : Switch.t * bool * Unix.file_descr -> Datagram_socket.t Effect.t
  | Socketpair_stream : Switch.t * Unix.socket_domain * int ->
      (Stream_socket.t * Stream_socket.t) Effect.t
  | Socketpair_datagram : Switch.t * Unix.socket_domain * int ->
      (Datagram_socket.t * Datagram_socket.t) Effect.t

let import_socket_stream ~sw ~close_unix fd =
  Effect.perform (Import_socket_stream (sw, close_unix, fd))

let import_socket_listening ~sw ~close_unix fd =
  Effect.perform (Import_socket_listening (sw, close_unix, fd))

let import_socket_datagram ~sw ~close_unix fd =
  Effect.perform (Import_socket_datagram (sw, close_unix, fd))

let socketpair_stream ~sw ?(domain=Unix.PF_UNIX) ?(protocol=0) () =
  Effect.perform (Socketpair_stream (sw, domain, protocol))

let socketpair_datagram ~sw ?(domain=Unix.PF_UNIX) ?(protocol=0) () =
  Effect.perform (Socketpair_datagram (sw, domain, protocol))
