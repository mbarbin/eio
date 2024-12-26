(** Extension of {!Eio} for integration with OCaml's [Unix] module.

    Note that OCaml's [Unix] module is not safe, and therefore care must be taken when using these functions.
    For example, it is possible to leak file descriptors this way, or to use them after they've been closed,
    allowing one module to corrupt a file belonging to an unrelated module. *)

[@@@alert "-unstable"]

open Eio.Std

type Eio.Exn.Backend.t += Unix_error of Unix.error * string * string
(** Wrapper for embedding {!Unix.Unix_error} errors. *)

module Fd = Fd
(** A safe wrapper for {!Unix.file_descr}. *)

module Source = Source
module Sink = Sink
module Flow = Flow

module Net = Net
(** Extended network API with support for file descriptors. *)

val await_readable : Unix.file_descr -> unit
(** [await_readable fd] blocks until [fd] is readable (or has an error). *)

val await_writable : Unix.file_descr -> unit
(** [await_writable fd] blocks until [fd] is writable (or has an error). *)

val sleep : float -> unit
(** [sleep d] sleeps for [d] seconds, allowing other fibers to run.
    This is can be useful for debugging (e.g. to introduce delays to trigger a race condition)
    without having to plumb {!Eio.Stdenv.mono_clock} through your code.
    It can also be used in programs that don't care about tracking determinism. *)

val run_in_systhread : ?label:string -> (unit -> 'a) -> 'a
(** [run_in_systhread fn] runs the function [fn] using a pool of system threads ({! Thread.t}).

    This pool creates a new system thread if all threads are busy, it does not wait.
    [run_in_systhread] allows blocking calls to be made non-blocking.

    @param label The operation name to use in trace output. *)

val pipe : Switch.t -> Source.r * Sink.r
(** [pipe sw] returns a connected pair of flows [src] and [sink]. Data written to [sink]
    can be read from [src].
    Note that, like all FDs created by Eio, they are both marked as close-on-exec by default. *)

module Process = Process
(** Spawning child processes with extra control. *)

module Cap = Cap
(** Capsicum security. *)

(** The set of resources provided to a process on a Unix-compatible system. *)
module Stdenv : sig
  type base = <
    stdin  : Eio.Flow.Source.r;
    stdout : Eio.Flow.Sink.r;
    stderr : Eio.Flow.Sink.r;
    net : Net.r;
    domain_mgr : Eio.Domain_manager.t;
    process_mgr : Process.mgr_r;
    clock : float Eio.Time.clock_ty r;
    mono_clock : Eio.Time.Mono.ty r;
    fs : Eio.Path.t;
    cwd : Eio.Path.t;
    secure_random : Eio.Flow.Source.r;
    debug : Eio.Debug.t;
    backend_id : string;
  >
  (** The common set of features provided by all traditional operating systems (BSDs, Linux, Mac, Windows).

      You can use the functions in {!Eio.Stdenv} to access these fields if you prefer. *)
end

(** API for Eio backends only. *)
module Private : sig
  type _ Effect.t +=
    | Await_readable : Unix.file_descr -> unit Effect.t      (** See {!await_readable} *)
    | Await_writable : Unix.file_descr -> unit Effect.t      (** See {!await_writable} *)
    | Get_monotonic_clock : Eio.Time.Mono.ty r Effect.t
    | Pipe : Eio.Switch.t -> (Source.r * Sink.r) Effect.t    (** See {!pipe} *)

  module Rcfd = Rcfd

  module Fork_action = Fork_action

  module Thread_pool = Thread_pool

  val read_link : Fd.t option -> string -> string
  val read_link_unix : Unix.file_descr option -> string -> string
end
