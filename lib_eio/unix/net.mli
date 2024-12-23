open Eio.Std

(** {2 Types}

    These extend the types in {!Eio.Net} with support for file descriptors. *)

module Stream_socket = Stream_socket
module Listening_socket = Listening_socket
module Datagram_socket = Datagram_socket

module type S = sig
  type t

  val listen :
    t -> reuse_addr:bool -> reuse_port:bool -> backlog:int -> sw:Switch.t ->
    Eio.Net.Sockaddr.stream -> Listening_socket.r

  val connect : t -> sw:Switch.t -> Eio.Net.Sockaddr.stream -> Stream_socket.r

  val datagram_socket :
    t
    -> reuse_addr:bool
    -> reuse_port:bool
    -> sw:Switch.t
    -> [Eio.Net.Sockaddr.datagram | `UdpV4 | `UdpV6]
    -> Datagram_socket.r

  val getaddrinfo : t -> service:string -> string -> Eio.Net.Sockaddr.t list
  val getnameinfo : t -> Eio.Net.Sockaddr.t -> (string * string)
end

(* CR mbarbin: Remark about the scheme used: it should be possible to
   remove the tuple from what's encapsulated by the ['r] variable
   (only taking the object part). I suspect this may be simpler, but I
   am not confident about, and not confident whether that'd work
   either, this is just an idea atm. *)
type ('a, 'r) t =
  ('a *
   (< network : (module Eio.Net.NETWORK with type t = 'a)
    ; network_unix : (module S with type t = 'a)
    ; .. > as 'r))

val accept :
  sw:Switch.t ->
  _ Listening_socket.t ->
  Stream_socket.r * Eio.Net.Sockaddr.stream

val listen :
  ?reuse_addr:bool -> ?reuse_port:bool -> backlog:int -> sw:Switch.t ->
 _ t -> Eio.Net.Sockaddr.stream -> Listening_socket.r

val connect : sw:Switch.t -> _ t -> Eio.Net.Sockaddr.stream -> Stream_socket.r

module Pi : sig
  val make : (module S with type t = 'a) -> 'a ->
  ('a,
  (< network : (module Eio.Net.NETWORK with type t = 'a)
   ; network_unix : (module S with type t = 'a)
   >)) t
end

(** {2 Passing file descriptors} *)

val send_msg :
  _ Stream_socket.t ->
  ?fds:Fd.t list ->
  Cstruct.t list -> unit
(** Like {!Eio.Flow.write}, but allows passing file descriptors (for Unix-domain sockets). *)

val recv_msg_with_fds :
  _ Stream_socket.t ->
  sw:Switch.t ->
  max_fds:int ->
  Cstruct.t list ->
  int * Fd.t list
(** Like {!Eio.Flow.single_read}, but also allows receiving file descriptors (for Unix-domain sockets).

    @param max_fds The maximum number of file descriptors to accept (additional ones will be closed). *)

(** {2 Unix address conversions}

    Note: OCaml's {!Unix.sockaddr} type considers e.g. TCP port 80 and UDP port
    80 to be the same thing, whereas Eio regards them as separate addresses
    that just happen to have the same representation (a host address and a port
    number), so we have separate "of_unix" functions for each. *)

val sockaddr_to_unix : [< Eio.Net.Sockaddr.stream | Eio.Net.Sockaddr.datagram] -> Unix.sockaddr
val sockaddr_of_unix_stream : Unix.sockaddr -> Eio.Net.Sockaddr.stream
val sockaddr_of_unix_datagram : Unix.sockaddr -> Eio.Net.Sockaddr.datagram

(** Convert between Eio.Net.Ipaddr and Unix.inet_addr. *)
module Ipaddr : sig
  (** Internally, these are actually the same type, so these are just casts. *)

  val to_unix : [< `V4 | `V6] Eio.Net.Ipaddr.t -> Unix.inet_addr
  val of_unix : Unix.inet_addr -> Eio.Net.Ipaddr.v4v6
end

(** {2 Creating or importing sockets} *)

val import_socket_stream : sw:Switch.t -> close_unix:bool -> Unix.file_descr -> Stream_socket.r
(** [import_socket_stream ~sw ~close_unix fd] is an Eio flow that uses [fd].

    It can be cast to e.g. {!source} for a one-way flow.
    The socket object will be closed when [sw] finishes.

    The [close_unix] and [sw] arguments are passed to {!Fd.of_unix}. *)

val import_socket_listening : sw:Switch.t -> close_unix:bool -> Unix.file_descr -> Listening_socket.r
(** [import_socket_listening ~sw ~close_unix fd] is an Eio listening socket that uses [fd].

    The socket object will be closed when [sw] finishes.

    The [close_unix] and [sw] arguments are passed to {!Fd.of_unix}. *)

val import_socket_datagram : sw:Switch.t -> close_unix:bool -> Unix.file_descr -> Datagram_socket.r
(** [import_socket_datagram ~sw ~close_unix fd] is an Eio datagram socket that uses [fd].

    The socket object will be closed when [sw] finishes.

    The [close_unix] and [sw] arguments are passed to {!Fd.of_unix}. *)

val socketpair_stream :
  sw:Switch.t ->
  ?domain:Unix.socket_domain ->
  ?protocol:int ->
  unit ->
  Stream_socket.r * Stream_socket.r
(** [socketpair_stream ~sw ()] returns a connected pair of flows, such that writes to one can be read by the other.

    This creates OS-level resources using [socketpair(2)].
    Note that, like all FDs created by Eio, they are both marked as close-on-exec by default. *)

val socketpair_datagram :
  sw:Switch.t ->
  ?domain:Unix.socket_domain ->
  ?protocol:int ->
  unit ->
  Datagram_socket.r * Datagram_socket.r
(** [socketpair_datagram ~sw ()] returns a connected pair of flows, such that writes to one can be read by the other.

    This creates OS-level resources using [socketpair(2)].
    Note that, like all FDs created by Eio, they are both marked as close-on-exec by default. *)

(** {2 Private API for backends} *)

val getnameinfo : Eio.Net.Sockaddr.t -> (string * string)
(** [getnameinfo sockaddr] returns domain name and service for [sockaddr]. *)

type _ Effect.t +=
  | Import_socket_stream :
      Switch.t * bool * Unix.file_descr -> Stream_socket.r Effect.t     (** See {!import_socket_stream} *)
  | Import_socket_listening :
      Switch.t * bool * Unix.file_descr -> Listening_socket.r Effect.t  (** See {!import_socket_listening} *)
  | Import_socket_datagram :
      Switch.t * bool * Unix.file_descr -> Datagram_socket.r Effect.t   (** See {!import_socket_datagram} *)
  | Socketpair_stream : Eio.Switch.t * Unix.socket_domain * int ->
      (Stream_socket.r * Stream_socket.r) Effect.t      (** See {!socketpair_stream} *)
  | Socketpair_datagram : Eio.Switch.t * Unix.socket_domain * int ->
      (Datagram_socket.r * Datagram_socket.r) Effect.t  (** See {!socketpair_datagram} *)
[@@alert "-unstable"]
