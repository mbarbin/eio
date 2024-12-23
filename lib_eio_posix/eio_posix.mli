(** Fallback Eio backend for POSIX systems. *)

type stdenv = 'net Eio_unix.Stdenv.base
  constraint 'net = int

(** The type of the standard set of resources available on POSIX systems. *)

val run : (stdenv -> 'a) -> 'a
(** [run main] runs an event loop and calls [main stdenv] inside it.

    For portable code, you should use {!Eio_main.run} instead, which will call this for you if appropriate. *)

module Low_level = Low_level
(** Low-level API for making POSIX calls directly. *)
