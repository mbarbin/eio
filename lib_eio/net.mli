(** Example:
    {[
      let addr = `Tcp (Ipaddr.V4.loopback, 8080)

      let http_get ~net ~stdout addr =
        Switch.run @@ fun sw ->
        let flow = Net.connect ~sw net addr in
        Flow.copy_string "GET / HTTP/1.0\r\n\r\n" flow;
        Flow.shutdown flow `Send;
        Flow.copy flow stdout
      ]}
*)

open Std

type connection_failure =
  | Refused of Exn.Backend.t
  | No_matching_addresses
  | Timeout

type error =
  | Connection_reset of Exn.Backend.t
    (** This is a wrapper for epipe, econnreset and similar errors.
        It indicates that the flow has failed, and data may have been lost. *)
  | Connection_failure of connection_failure

type Exn.err += E of error

val err : error -> exn
(** [err e] is [Eio.Exn.create (Net e)] *)

(** IP addresses. *)
module Ipaddr = Ipaddr

(** Network addresses. *)
module Sockaddr = Sockaddr

(** {2 Types} *)

module Stream_socket = Stream_socket

module Listening_socket = Listening_socket

type 'a connection_handler = 'a Stream_socket.t' -> Sockaddr.stream -> unit
(** A [_ connection_handler] handles incoming connections from a listening socket. *)

module Datagram_socket = Datagram_socket

module type NETWORK = sig
  type t

  val listen :
    t -> reuse_addr:bool -> reuse_port:bool -> backlog:int -> sw:Switch.t ->
    Sockaddr.stream -> _ Listening_socket.t

  val connect : t -> sw:Switch.t -> Sockaddr.stream -> _ Stream_socket.t

  val datagram_socket :
    t
    -> reuse_addr:bool
    -> reuse_port:bool
    -> sw:Switch.t
    -> [Sockaddr.datagram | `UdpV4 | `UdpV6]
    -> _ Datagram_socket.t

  val getaddrinfo : t -> service:string -> string -> Sockaddr.t list
  val getnameinfo : t -> Sockaddr.t -> (string * string)
end

type ('a, 'r) t =
  ('a *
   (< network : (module NETWORK with type t = 'a)
   ; ..
   > as 'r))

(** {2 Out-bound Connections} *)

val connect : sw:Switch.t -> _ t -> Sockaddr.stream -> _ Stream_socket.t
(** [connect ~sw t addr] is a new socket connected to remote address [addr].

    The new socket will be closed when [sw] finishes, unless closed manually first. *)

val with_tcp_connect :
  ?timeout:Time.Timeout.t ->
  host:string ->
  service:string ->
  _ t ->
  (_ Stream_socket.t -> 'b) ->
  'b
(** [with_tcp_connect ~host ~service t f] creates a tcp connection [conn] to [host] and [service] and executes 
    [f conn].

    [conn] is closed after [f] returns (if it isn't already closed by then).

    [host] is either an IP address or a domain name, eg. "www.example.org", "www.ocaml.org" or "127.0.0.1".

    [service] is an IANA recognized service name or port number, eg. "http", "ftp", "8080" etc.
    See https://www.iana.org/assignments/service-names-port-numbers/service-names-port-numbers.xhtml.

    Addresses are tried in the order they are returned by {!getaddrinfo}, until one succeeds.

    @param timeout Limits how long to wait for each connection attempt before moving on to the next.
                   By default there is no timeout (beyond what the underlying network does).

    @raise Connection_failure A connection couldn't be established for any of the addresses defined for [host]. *)

(** {2 Incoming Connections} *)

val listen :
  ?reuse_addr:bool -> ?reuse_port:bool -> backlog:int -> sw:Switch.t ->
  _ t -> Sockaddr.stream -> _ Listening_socket.t
(** [listen ~sw ~backlog t addr] is a new listening socket bound to local address [addr].

    The new socket will be closed when [sw] finishes, unless closed manually first.

    On platforms that support this, passing port [0] will bind to a random port.

    For (non-abstract) Unix domain sockets, the path will be removed afterwards.

    @param backlog The number of pending connections that can be queued up (see listen(2)).
    @param reuse_addr Set the {!Unix.SO_REUSEADDR} socket option.
                      For Unix paths, also remove any stale left-over socket.
    @param reuse_port Set the {!Unix.SO_REUSEPORT} socket option. *)

val accept :
  sw:Switch.t ->
  _ Listening_socket.t ->
  _ Stream_socket.t * Sockaddr.stream
(** [accept ~sw socket] waits until a new connection is ready on [socket] and returns it.

    The new socket will be closed automatically when [sw] finishes, if not closed earlier.
    If you want to handle multiple connections, consider using {!accept_fork} instead. *)

val accept_fork :
  sw:Switch.t ->
  _ Listening_socket.t ->
  on_error:(exn -> unit) ->
  _ Stream_socket.t connection_handler ->
  unit
(** [accept_fork ~sw ~on_error socket fn] accepts a connection and handles it in a new fiber.

    After accepting a connection to [socket], it runs [fn flow client_addr] in a new fiber.

    [flow] will be closed when [fn] returns. The new fiber is attached to [sw].

    @param on_error Called if [connection_handler] raises an exception.
                    This is typically a good place to log the error and continue.
                    If the exception is an {!Eio.Io} error then the caller's address is added to it.

                    If you don't want to handle connection errors,
                    use [~on_error:raise] to cancel the caller's context.

                    [on_error] is not called for {!Cancel.Cancelled} exceptions,
                    which do not need to be reported. *)

val listening_addr : _ Listening_socket.t -> Sockaddr.stream

(** {2 Running Servers} *)

val run_server :
  ?max_connections:int ->
  ?additional_domains:(Domain_manager.t * int) ->
  ?stop:'a Promise.t ->
  on_error:(exn -> unit) ->
  _ Listening_socket.t ->
  _ Stream_socket.t connection_handler ->
  'a
(** [run_server ~on_error sock connection_handler] establishes a concurrent socket server [s].

    It accepts incoming client connections on socket [sock] and handles them with {!accept_fork}
    (see that for the description of [on_error] and [connection_handler]).

    {b Running a Parallel Server}

    By default [s] runs on a {e single} OCaml {!module:Domain}. However, if [additional_domains:(domain_mgr, domains)]
    parameter is given, then [s] will spawn [domains] additional domains and run accept loops in those too.
    In such cases you must ensure that [connection_handler] only accesses thread-safe values.
    Note that having more than {!Domain.recommended_domain_count} domains in total is likely to result in bad performance.

    @param max_connections The maximum number of concurrent connections accepted by [s] at any time.
                           The default is [Int.max_int].
    @param stop Resolving this promise causes [s] to stop accepting new connections.
                [run_server] will wait for all existing connections to finish and then return.
                This is useful to upgrade a server without clients noticing.
                To stop immediately, cancelling all connections, just cancel [s]'s fiber instead.
    @param on_error Connection error handler (see {!accept_fork}).
    @raise Invalid_argument if [max_connections <= 0].
                            if [additional_domains = (domain_mgr, domains)] is used and [domains < 0]. *)

(** {2 Datagram Sockets} *)

val datagram_socket :
     ?reuse_addr:bool
  -> ?reuse_port:bool
  -> sw:Switch.t
  -> _ t
  -> [< Sockaddr.datagram | `UdpV4 | `UdpV6]
  -> _ Datagram_socket.t
  (** [datagram_socket ~sw t addr] creates a new datagram socket bound to [addr]. The new 
      socket will be closed when [sw] finishes. 

      [`UdpV4] and [`UdpV6] represents IPv4 and IPv6
      datagram client sockets where the OS assigns the next available socket address and port
      automatically. [`Udp ..] can be used to create both listening server socket and client 
      socket.

      @param reuse_addr Set the {!Unix.SO_REUSEADDR} socket option.
      @param reuse_port Set the {!Unix.SO_REUSEPORT} socket option. *)

val send : _ Datagram_socket.t -> ?dst:Sockaddr.datagram -> Cstruct.t list -> unit
(** [send sock buf] sends the data in [buf] using the the datagram socket [sock].

    @param dst If [sock] isn't connected, this provides the destination. *)

val recv : _ Datagram_socket.t -> Cstruct.t -> Sockaddr.datagram * int
(** [recv sock buf] receives data from the socket [sock] putting it in [buf]. The number of bytes received is 
    returned along with the sender address and port. If the [buf] is too small then excess bytes may be discarded
    depending on the type of the socket the message is received from. *)

(** {2 DNS queries} *)

val getaddrinfo: ?service:string -> _ t -> string -> Sockaddr.t list
(** [getaddrinfo ?service t node] returns a list of IP addresses for [node]. [node] is either a domain name or
    an IP address.

    @param service is a human friendly textual name for internet services assigned by IANA., eg.
    'http', 'https', 'ftp', etc.

    For a more thorough treatment, see {{:https://man7.org/linux/man-pages/man3/getaddrinfo.3.html} getaddrinfo}. *)

val getaddrinfo_stream: ?service:string -> _ t -> string -> Sockaddr.stream list
(** [getaddrinfo_stream] is like {!getaddrinfo}, but filters out non-stream protocols. *)

val getaddrinfo_datagram: ?service:string -> _ t -> string -> Sockaddr.datagram list
(** [getaddrinfo_datagram] is like {!getaddrinfo}, but filters out non-datagram protocols. *)

val getnameinfo : _ t -> Sockaddr.t -> (string * string)
(** [getnameinfo t sockaddr] is [(hostname, service)] corresponding to [sockaddr]. [hostname] is the
    registered domain name represented by [sockaddr]. [service] is the IANA specified textual name of the
    port specified in [sockaddr], e.g. 'ftp', 'http', 'https', etc. *)

(** {2 Closing} *)

val close : [> `Close] r -> unit
(** Alias of {!Resource.close}. *)

(** {2 Provider Interface} *)

module Pi : sig
  val network
    : (module NETWORK with type t = 'a)
    -> 'a
    -> ('a, < network : (module NETWORK with type t = 'a) >) t
end
