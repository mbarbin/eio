(** Flows are used to represent byte streams, such as open files and network sockets.
    A {!source} provides a stream of bytes. A {!sink} consumes a stream.
    A {!two_way} can do both.

    To read structured data (e.g. a line at a time), wrap a source using {!Buf_read}. *)

open Std

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

module type Read_S = sig
  type source

  val single_read : source -> Cstruct.t -> int
  (** [single_read src buf] reads one or more bytes into [buf].

      It returns the number of bytes read (which may be less than the
      buffer size even if there is more data to be read).

      - Use {!read_exact} instead if you want to fill [buf] completely.
      - Use {!Buf_read.line} to read complete lines.
      - Use {!copy} to stream data directly from a source to a sink.

      [buf] must not be zero-length.

      @raise End_of_file if there is no more data to read *)

  val read_exact : source -> Cstruct.t -> unit
  (** [read_exact src dst] keeps reading into [dst] until it is full.
      @raise End_of_file if the buffer could not be filled. *)
end

module Source = Source

include Read_S with type source := Source.t

val string_source : string -> Source.t
(** [string_source s] is a source that gives the bytes of [s]. *)

val cstruct_source : Cstruct.t list -> Source.t
(** [cstruct_source cs] is a source that gives the bytes of [cs]. *)

(** {2 Writing} *)

module type Write_S = sig
  type source
  type sink

  val write : sink -> Cstruct.t list -> unit
  (** [write dst bufs] writes all bytes from [bufs].

      You should not perform multiple concurrent writes on the same flow
      (the output may get interleaved).

      This is a low level API. Consider using:

      - {!Buf_write} to combine multiple small writes.
      - {!copy} for bulk transfers, as it allows some extra optimizations. *)

  val single_write : sink -> Cstruct.t list -> int
  (** [single_write dst bufs] writes at least one byte from [bufs] and returns the number of bytes written. *)

  val copy : source -> sink -> unit
  (** [copy src dst] copies data from [src] to [dst] until end-of-file. *)

  val copy_string : string -> sink -> unit
  (** [copy_string s = copy (string_source s)] *)
end

module Sink = Sink

include Write_S with type source := Source.t and type sink := Sink.t

val buffer_sink : Buffer.t -> Sink.t
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

type t =
  |  T :
      ('a *
       < source : (module Source.S with type t = 'a)
       ; sink : (module Sink.S with type t = 'a)
       ; shutdown : (module Shutdownable.S with type t = 'a)
       ; resource_store : 'a Resource_store.t
       ; ..>)
      -> t [@@unboxed]

module Two_way : sig
  include Read_S with type source := t
  include Write_S with type source := t and type sink := t
end

module Cast : sig
  val as_source : t -> Source.t
  val as_sink : t -> Sink.t
end

val shutdown : t -> shutdown_command -> unit
(** [shutdown t cmd] indicates that the caller has finished reading or writing [t]
    (depending on [cmd]).

    This is useful in some protocols to indicate that you have finished sending the request,
    and that the remote peer should now send the response. *)

(** {2 Closing}

    Flows are usually attached to switches and closed automatically when the switch
    finishes. However, it can be useful to close them sooner manually in some cases. *)

(* CR mbarbin: Remove. *)
val close : [> `Close] r -> unit
(** Alias of {!Resource.close}. *)

(* CR mbarbin: Review, think about the names for consistency. *)
module Closable : sig
  type closable_source =
    | Closable_source :
        ('a *
         < source : (module Source.S with type t = 'a)
         ; close : 'a -> unit
         ; resource_store : 'a Resource_store.t
         ; ..>)
        -> closable_source [@@unboxed]

  type closable_sink =
    | Closable_sink :
        ('a *
         < sink : (module Sink.S with type t = 'a)
         ; close : 'a -> unit
         ; resource_store : 'a Resource_store.t
         ; ..>)
        -> closable_sink [@@unboxed]

  val source : closable_source -> Source.t
  val sink : closable_sink -> Sink.t
end

val close_source : Closable.closable_source -> unit
(** [close_source src] closes the source. *)

val close_sink : Closable.closable_sink -> unit
(** [close_sink dst] closes the sink. *)

(** {2 Provider Interface} *)

module Pi : sig
  (* CR mbarbin: Once the module are refactored, move this into their own module. E.g. [Source.Pi.make]. *)
  val make : (module S with type t = 'a) -> 'a -> t
  val simple_copy : single_write:('t -> Cstruct.t list -> int) -> 't -> src:Source.t -> unit
  (** [simple_copy ~single_write] implements {!SINK}'s [copy] API using [single_write]. *)
end

(** {2 Type aliases} *)

(* CR mbarbin: Decide what to do with the aliases. Could reduce breakages, but it's confusing
   to have both available. *)

type source = Source.t
type sink = Sink.t
