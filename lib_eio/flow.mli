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

(* CR mbarbin: Possibly it is better to return a Source.packed for
   these constructors. *)
module String_source : sig
  type t
end

val string_source : string -> String_source.t Source.t'
(** [string_source s] is a source that gives the bytes of [s]. *)

module Cstruct_source : sig
  type t
end

val cstruct_source : Cstruct.t list -> Cstruct_source.t Source.t'
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

val buffer_sink : Buffer.t -> _ Sink.t'
(** [buffer_sink b] is a sink that adds anything sent to it to [b].

    To collect data as a cstruct, use {!Buf_read} instead. *)

type shutdown_command = Shutdownable.shutdown_command

module type SOURCE = Source.S
module type SINK = Sink.S
module type SHUTDOWN = Shutdownable.S

module type S = sig
  include Shutdownable.S
  include Source.S with type t := t
  include Sink.S with type t := t
end

(** {2 Bidirectional streams} *)

type ('a, 'r) t =
  ('a *
   < source : (module Source.S with type t = 'a)
   ; sink : (module Sink.S with type t = 'a)
   ; shutdown : (module Shutdownable.S with type t = 'a)
   ; resource_store : 'a Resource_store.t
   ; ..> as 'r)

val shutdown : _ t -> shutdown_command -> unit
(** [shutdown t cmd] indicates that the caller has finished reading or writing [t]
    (depending on [cmd]).

    This is useful in some protocols to indicate that you have finished sending the request,
    and that the remote peer should now send the response. *)

(** {2 Closing}

    Flows are usually attached to switches and closed automatically when the switch
    finishes. However, it can be useful to close them sooner manually in some cases. *)

(* CR mbarbin: Review, think about the names for consistency. *)
type ('a, 'r) closable_source =
  ('a *
   < source : (module Source.S with type t = 'a)
   ; close : 'a -> unit
   ; resource_store : 'a Resource_store.t
   ; ..> as 'r)

type ('a, 'r) closable_sink =
  ('a *
   < sink : (module Sink.S with type t = 'a)
   ; close : 'a -> unit
   ; resource_store : 'a Resource_store.t
   ; ..> as 'r)

val close_source : _ closable_source -> unit
(** [close_source src] closes the source. *)

val close_sink : _ closable_sink -> unit
(** [close_sink dst] closes the sink. *)

(** {2 Provider Interface} *)

module Pi : sig
  (* CR mbarbin: Perhaps this should simply be [make] at the toplevel of the module. *)
  val make : (module S with type t = 'a) -> 'a ->
    ('a, 'a *
     < source : (module Source.S with type t = 'a)
     ; sink : (module Sink.S with type t = 'a)
     ; shutdown : (module Shutdownable.S with type t = 'a)
     ; resource_store : 'a Resource_store.t
     >) t

  val simple_copy : single_write:('t -> Cstruct.t list -> int) -> 't -> src:_ Source.t -> unit
  (** [simple_copy ~single_write] implements {!SINK}'s [copy] API using [single_write]. *)
end

(** {2 Type aliases} *)

(* CR mbarbin: Decide what to do with the aliases. Could reduce breakages, but it's confusing
   to have both available. *)

type ('a, 'r) source = ('a, 'r) Source.t
type ('a, 'r) sink = ('a, 'r) Sink.t
