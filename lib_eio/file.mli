(** Files implement the {!Flow} APIs, which can be used for reading and writing data.
    This module provides additonal file-specific operations, such as seeking within a file.

    To get an open file, use the functions in the {!Path} module. *)

(** {2 Types} *)

(** Traditional Unix permissions. *)
module Unix_perm = Stat.Unix_perm

(** Portable file stats. *)
module Stat = Stat

type ('a, 'r) ro = ('a, 'r) File_ro.t
(** A file opened for reading. *)

type ('a, 'r) rw = ('a, 'r) File_rw.t
(** A file opened for reading and writing. *)

(** {2 Metadata} *)

val stat : _ ro -> Stat.t
(** [stat t] returns the {!Stat.t} record associated with [t]. *)

val size : _ ro -> Optint.Int63.t
(** [size t] returns the size of [t]. *)

(** {2 Reading and writing} *)

val pread : _ ro -> file_offset:Optint.Int63.t -> Cstruct.t list -> int
(** [pread t ~file_offset bufs] performs a single read of [t] at [file_offset] into [bufs].

    It returns the number of bytes read, which may be less than the space in [bufs],
    even if more bytes are available. Use {!pread_exact} instead if you require
    the buffer to be filled.

    To read at the current offset, use {!Flow.single_read} instead. *)

val pread_exact : _ ro -> file_offset:Optint.Int63.t -> Cstruct.t list -> unit
(** [pread_exact t ~file_offset bufs] reads from [t] into [bufs] until [bufs] is full.

    @raise End_of_file if the buffer could not be filled. *)

val pwrite_single : _ rw -> file_offset:Optint.Int63.t -> Cstruct.t list -> int
(** [pwrite_single t ~file_offset bufs] performs a single write operation, writing
    data from [bufs] to location [file_offset] in [t].

    It returns the number of bytes written, which may be less than the length of [bufs].
    In most cases, you will want to use {!pwrite_all} instead. *)

val pwrite_all : _ rw -> file_offset:Optint.Int63.t -> Cstruct.t list -> unit
(** [pwrite_all t ~file_offset bufs] writes all the data in [bufs] to location [file_offset] in [t]. *)

val seek : _ ro -> Optint.Int63.t -> [`Set | `Cur | `End] -> Optint.Int63.t
(** Set and/or get the current file position.

    Like {!Unix.lseek}. *)

val sync : _ rw -> unit
(** Flush file buffers to disk.

    Like {!Unix.fsync}. *)

val truncate : _ rw -> Optint.Int63.t -> unit
(** Set the length of a file.

    Like {!Unix.ftruncate}. *)

(** {2 Provider Interface} *)

module File_ro = File_ro
module File_rw = File_rw
