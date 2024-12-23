(** Mocks for testing.

    When testing an Eio program it is often convenient to use mock resources rather than real OS-provided ones.
    This allows precise control over the test, such as adding delays or simulated faults.
    You can always just implement the various Eio types directly,
    but this module provides some convenient pre-built mocks, and some helpers for creating your own mocks.

    Mocks typically use {!Eio.traceln} to record how they were used.
    This output can be recorded and compared against a known-good copy using e.g.
    {{:https://github.com/realworldocaml/mdx}ocaml-mdx}.

    Mocks may require configuration.
    For example, a source flow needs to know what data to return when the application reads from it.
    This can be done using the various [on_*] functions. For example:

    {[
      let stdin = Eio_mock.Flow.make "stdin" in
      let stdout = Eio_mock.Flow.make "stdout" in
      Eio_mock.Flow.on_read stdin [
        `Return "chunk1";
        `Return "chunk2";
        `Raise End_of_file
      ];
      Eio.Flow.copy stdin stdout
    ]}

    This will produce:

    {[
      +stdin: read "chunk1"
      +stdout: wrote "chunk1"
      +stdin: read "chunk2"
      +stdout: wrote "chunk2"
    ]}
*)

(** {2 Configuration} *)

(** Actions that can be performed by mock handlers. *)
module Action : sig
  type 'a t = [
    | `Return of 'a                     (** Immediately return a value *)
    | `Raise of exn                     (** Raise an exception *)
    | `Await of 'a Eio.Promise.or_exn   (** Wait for a promise to resolve *)
    | `Yield_then of 'a t               (** Call {!Eio.Fiber.yield}, then perform an action *)
    | `Run of unit -> 'a                (** Run any code you like. *)
  ]

  val run : 'a t -> 'a
  (** [run t] performs action [t] and returns the result. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [run (map f t) = f (run t)]. *)
end

(** Control how a mock responds.

    This module is mostly useful when writing custom mocks.
    Individual mocks usually provide convenience wrappers around this. *)
module Handler : sig
  type 'a t
  (** A handler that provides values of type ['a]. *)

  type 'a actions = 'a Action.t list

  val make : 'a Action.t -> 'a t
  (** [make default_action] is a new handler that initially always runs [default_action]. *)

  val set_handler : 'a t -> (unit -> 'a) -> unit
  (** [set_handler t fn] sets (replaces) the function to be called whenever the handler is run. *)

  val seq : 'a t -> 'a actions -> unit
  (** [seq t actions] sets a handler function that performs the next action in [actions] on each call.
      When there are no more actions, it runs the default handler. *)

  val run : 'a t -> 'a
  (** [run t] is used by mocks to run their handlers. *)

  val run_default_action : 'a t -> 'a
  (** [run_default_action t] runs the default handler passed to {!make}. *)
end

(** {2 Pre-defined mocks} *)

(** Mock {!Eio.Flow} sources and sinks. *)
module Flow : sig
  module Mock_flow : sig
    type t
  end

  type copy_method = [
    | `Read_into                (** Use the source's [read_into] method (the default). *)
    | `Read_source_buffer       (** Use the {!Eio.Flow.Read_source_buffer} optimisation. *)
  ]

  class type ['a] flow = object
    method raw : 'a -> Mock_flow.t
    method shutdown : (module Eio.Flow.SHUTDOWN with type t = 'a)
    method source : (module Eio.Flow.SOURCE with type t = 'a)
    method sink : (module Eio.Flow.SINK with type t = 'a)
    method close : 'a -> unit
    method resource_store : 'a Eio.Resource_store.t
  end

  type ('a, 'r) t =
    ('a *
     (< raw : 'a -> Mock_flow.t
      ; shutdown : (module Eio.Flow.SHUTDOWN with type t = 'a)
      ; source : (module Eio.Flow.SOURCE with type t = 'a)
      ; sink : (module Eio.Flow.SINK with type t = 'a)
      ; close : 'a -> unit
      ; resource_store : 'a Eio.Resource_store.t
      ; ..> as 'r))

  type 'a t' = ('a, 'a flow) t

  type r = T : 'a t' -> r [@@unboxed]

  val make : ?pp:string Fmt.t -> string -> Mock_flow.t t'
  (** [make label] is a mock Eio flow.
      It can be used as a source, sink, or two-way flow.
      @param pp Printer to use to display the data. *)

  val on_read : _ t -> string Handler.actions -> unit
  (** [on_read t actions] configures the values to return from the mock's [read] function. *)

  val on_copy_bytes : _ t -> int Handler.actions -> unit
  (** [on_copy_bytes t actions] configures the number of bytes to copy in each iteration. *)

  val set_copy_method : _ t -> copy_method -> unit
  (** [set_copy_method t m] configures [t] to use the given method to read from
      a source during a copy operation. *)

  val close : _ t -> unit

  module Cast : sig
    val as_stream_socket : r -> Eio.Net.Stream_socket.r
  end
end

(** Mock {!Eio.Net} networks and sockets. *)
module Net : sig
  module Impl : sig
    type t
  end

  module Listening_socket_impl : sig
    type t
  end

  class type ['a] network = object
    method network : (module Eio.Net.NETWORK with type t = 'a)
    method raw : 'a -> Impl.t
  end

  type ('a, 'r) t =
    ('a *
     (< network : (module Eio.Net.NETWORK with type t = 'a)
      ; raw : 'a -> Impl.t
      ; ..> as 'r))

  type 'a t' = ('a, 'a network) t

  type r = T : 'a t' -> r [@@unboxed]

  module Listening_socket : sig
    class type ['a] listening_socket = object
      method listening_socket : (module Eio.Net.Listening_socket.S with type t = 'a)
      method close : 'a -> unit
      method resource_store : 'a Eio.Resource_store.t
      method raw : 'a -> Listening_socket_impl.t
    end

    type ('a, 'r) t =
      ('a *
       (< listening_socket : (module Eio.Net.Listening_socket.S with type t = 'a)
        ; close : 'a -> unit
        ; resource_store : 'a Eio.Resource_store.t
        ; raw : 'a -> Listening_socket_impl.t
        ; ..> as 'r))

    type 'a t' = ('a, 'a listening_socket) t

    type r = T : 'a t' -> r
  end

  val make : string -> Impl.t t'
  (** [make label] is a new mock network. *)

  val on_connect : _ t -> Eio.Net.Stream_socket.r Handler.actions -> unit
  (** [on_connect t actions] configures what to do when a client tries to connect somewhere. *)

  val on_listen : _ t -> Eio.Net.Listening_socket.r Handler.actions -> unit
  (** [on_listen t actions] configures what to do when a server starts listening for incoming connections. *)

  val on_datagram_socket : _ t -> Eio.Net.Datagram_socket.r Handler.actions -> unit
  (** [on_datagram_socket t actions] configures how to create datagram sockets. *)

  val on_getaddrinfo : _ t -> Eio.Net.Sockaddr.t list Handler.actions -> unit

  val on_getnameinfo : _ t -> (string * string) Handler.actions -> unit

  val listening_socket :
    ?listening_addr:Eio.Net.Sockaddr.stream -> string -> Listening_socket.r
  (** [listening_socket label] can be configured to provide mock connections.

      If [listening_addr] is not provided, a dummy value will be reported. *)

  val on_accept :
    _ Listening_socket.t ->
    (Flow.r * Eio.Net.Sockaddr.stream) Handler.actions ->
    unit
  (** [on_accept socket actions] configures how to respond when the server calls "accept". *)

  module Cast : sig
    val as_generic : r -> Eio.Net.r
  end
end

(** A mock {!Eio.Time} clock for testing timeouts. *)
module Clock = Clock

(** A mock {!Eio.Domain_manager} that runs everything in a single domain. *)
module Domain_manager = Domain_manager

(** {2 Backend for mocks}

    The mocks can be used with any backend, but if you don't need any IO then you can use this one
    to avoid a dependency on eio_main. *)

module Backend = Backend

(** {2 Mock errors} *)

type Eio.Exn.Backend.t += Simulated_failure
(** A fake error code you can use for simulated faults. *)
