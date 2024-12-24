(** Flows are used to represent byte streams, such as open files and network sockets.
    A {!source} provides a stream of bytes. A {!sink} consumes a stream.
    A {!two_way} can do both.

    To read structured data (e.g. a line at a time), wrap a source using {!Buf_read}. *)

(** {2 Types} *)

type 'a read_method = 'a Read_method.t = ..
(** Sources can offer a list of ways to read them, in order of preference. *)

(** {2 Reading} *)

type 't read_method += Read_source_buffer of ('t -> (Cstruct.t list -> int) -> unit)
(** If a source offers [Read_source_buffer rsb] then the user can call [rsb t fn]
    to borrow a view of the source's buffers. [fn] returns the number of bytes it consumed.

    [rsb] will raise [End_of_file] if no more data will be produced.
    If no data is currently available, [rsb] will wait for some to become available before calling [fn].

    [fn] must not continue to use the buffers after it returns. *)

module Source = Source

val single_read : _ Source.t -> Cstruct.t -> int
(** [single_read src buf] reads one or more bytes into [buf].

      It returns the number of bytes read (which may be less than the
      buffer size even if there is more data to be read).

    - Use {!read_exact} instead if you want to fill [buf] completely.
    - Use {!Buf_read.line} to read complete lines.
    - Use {!copy} to stream data directly from a source to a sink.

      [buf] must not be zero-length.

      @raise End_of_file if there is no more data to read *)

val read_exact : _ Source.t -> Cstruct.t -> unit
(** [read_exact src dst] keeps reading into [dst] until it is full.
      @raise End_of_file if the buffer could not be filled. *)

type string_source

val string_source : string -> string_source Source.t'
(** [string_source s] is a source that gives the bytes of [s]. *)

type cstruct_source

val cstruct_source : Cstruct.t list -> cstruct_source Source.t'
(** [cstruct_source cs] is a source that gives the bytes of [cs]. *)

(** {2 Writing} *)

module Sink = Sink

val write : _ Sink.t -> Cstruct.t list -> unit
(** [write dst bufs] writes all bytes from [bufs].

      You should not perform multiple concurrent writes on the same flow
      (the output may get interleaved).

      This is a low level API. Consider using:

    - {!Buf_write} to combine multiple small writes.
    - {!copy} for bulk transfers, as it allows some extra optimizations. *)

val single_write : _ Sink.t -> Cstruct.t list -> int
(** [single_write dst bufs] writes at least one byte from [bufs] and returns the number of bytes written. *)

val copy : _ Source.t -> _ Sink.t -> unit
(** [copy src dst] copies data from [src] to [dst] until end-of-file. *)

val copy_string : string -> _ Sink.t -> unit
(** [copy_string s = copy (string_source s)] *)

type buffer_sink

val buffer_sink : Buffer.t -> buffer_sink Sink.t'
(** [buffer_sink b] is a sink that adds anything sent to it to [b].

    To collect data as a cstruct, use {!Buf_read} instead. *)

type shutdown_command = Shutdownable.shutdown_command

module type SOURCE = Source.S
module type SINK = Sink.S
module type SHUTDOWN = Shutdownable.S

(** {2 Bidirectional streams} *)

module type S = sig
  include Shutdownable.S
  include Source.S with type t := t
  include Sink.S with type t := t
end

class type ['a] flow = object
  method source : (module Source.S with type t = 'a)
  method sink : (module Sink.S with type t = 'a)
  method shutdown : (module Shutdownable.S with type t = 'a)
  method resource_store : 'a Resource_store.t
end

type ('a, 'r) t =
  ('a *
   (< source : (module Source.S with type t = 'a)
    ; sink : (module Sink.S with type t = 'a)
    ; shutdown : (module Shutdownable.S with type t = 'a)
    ; resource_store : 'a Resource_store.t
    ; ..> as 'r))

type 'a t' = ('a, 'a flow) t

type r = T : 'a t' -> r [@@unboxed]

val shutdown : _ t -> shutdown_command -> unit
(** [shutdown t cmd] indicates that the caller has finished reading or writing [t]
    (depending on [cmd]).

    This is useful in some protocols to indicate that you have finished sending the request,
    and that the remote peer should now send the response. *)

(** {2 Closing}

    Flows are usually attached to switches and closed automatically when the switch
    finishes. However, it can be useful to close them sooner manually in some cases. *)

module Closable_source : sig
  class type ['a] closable_source = object
    method source : (module Source.S with type t = 'a)
    method close : 'a -> unit
    method resource_store : 'a Resource_store.t
  end

  type ('a, 'r) t =
    ('a *
     (< source : (module Source.S with type t = 'a)
      ; close : 'a -> unit
      ; resource_store : 'a Resource_store.t
      ; ..> as 'r))

  type 'a t' = ('a, 'a closable_source) t

  type r = T : 'a t' -> r [@@unboxed]
end

module Closable_sink : sig
  class type ['a] closable_sink = object
    method sink : (module Sink.S with type t = 'a)
    method close : 'a -> unit
    method resource_store : 'a Resource_store.t
  end

  type ('a, 'r) t =
    ('a *
     (< sink : (module Sink.S with type t = 'a)
      ; close : 'a -> unit
      ; resource_store : 'a Resource_store.t
      ; ..> as 'r))

  type 'a t' = ('a, 'a closable_sink) t

  type r = T : 'a t' -> r [@@unboxed]
end

val close : _ Closable.t -> unit

(** {2 Provider Interface} *)

module Pi : sig
  (* CR mbarbin: Perhaps this should simply be [make] at the toplevel of the module. *)
  val make : (module S with type t = 'a) -> 'a -> 'a t'

  val simple_copy : single_write:('t -> Cstruct.t list -> int) -> 't -> src:_ Source.t -> unit
  (** [simple_copy ~single_write] implements {!SINK}'s [copy] API using [single_write]. *)
end

(** {2 Type aliases} *)

(* CR mbarbin: Decide what to do with the aliases. Could reduce
   breakages, but it's confusing to have both available. *)

type ('a, 'b) closable_source = ('a, 'b) Closable_source.t
type ('a, 'b) closable_sink = ('a, 'b) Closable_sink.t

type ('a, 'r) source = ('a, 'r) Source.t
type ('a, 'r) sink = ('a, 'r) Sink.t
