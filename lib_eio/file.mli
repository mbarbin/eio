(** Files implement the {!Flow} APIs, which can be used for reading and writing data.
    This module provides additonal file-specific operations, such as seeking within a file.

    To get an open file, use the functions in the {!Path} module. *)

(** {2 Types} *)

(** Traditional Unix permissions. *)
module Unix_perm : sig
  type t = int
  (** This is the same as {!Unix.file_perm}, but avoids a dependency on [Unix]. *)
end

(** Portable file stats. *)
module Stat : sig

  type kind = [
    | `Unknown
    | `Fifo
    | `Character_special
    | `Directory
    | `Block_device
    | `Regular_file
    | `Symbolic_link
    | `Socket
  ]
  (** Kind of file from st_mode. **)

  val pp_kind : kind Fmt.t
  (** Pretty printer for {! kind}. *)

  type t = {
    dev : Int64.t;              (** Device containing the filesystem where the file resides. *)
    ino : Int64.t;              (** Inode number. *)
    kind : kind;                (** File type. *)
    perm : Unix_perm.t;         (** Permissions (mode). *)
    nlink : Int64.t;            (** Number of hard links. *)
    uid : Int64.t;              (** User ID of owner. *)
    gid : Int64.t;              (** Group ID of owner. *)
    rdev : Int64.t;             (** Device's ID (if this is a device). *)
    size : Optint.Int63.t;      (** Total size in bytes. *)
    atime : float;              (** Last access time. *)
    mtime : float;              (** Last modification time. *)
    ctime : float;              (** Creation time. *)
  }
  (** Like stat(2). *)

  val pp : t Fmt.t
  (** Pretty printer for {! t}. *)
end

module type READ = sig
  include Flow.SOURCE

  val pread : t -> file_offset:Optint.Int63.t -> Cstruct.t list -> int
  val stat : t -> Stat.t
  val seek : t -> Optint.Int63.t -> [`Set | `Cur | `End] -> Optint.Int63.t
  val close : t -> unit
end

module type WRITE = sig
  include Flow.SINK
  include READ with type t := t

  val pwrite : t -> file_offset:Optint.Int63.t -> Cstruct.t list -> int
  val sync : t -> unit
  val truncate : t -> Optint.Int63.t -> unit
end

type ro = Ro : ('a *
  < read : (module READ with type t = 'a)
  ; source : (module Flow.SOURCE with type t = 'a)
  ; close : 'a -> unit
  ; .. >) -> ro [@@unboxed]
(** A file opened for reading. *)

module Ro : sig
  val to_source : ro -> Flow.source
end

type rw = Rw : ('a *
  < read : (module READ with type t = 'a)
  ; source : (module Flow.SOURCE with type t = 'a)
  ; close : 'a -> unit
  ; write : (module WRITE with type t = 'a)
  ; sink : (module Flow.SINK with type t = 'a)
  ; .. >) -> rw [@@unboxed]
(** A file opened for reading and writing. *)

module Rw : sig
  val to_ro : rw -> ro
  val to_sink : rw -> Flow.sink
end

(** {2 Metadata} *)

val stat : ro -> Stat.t
(** [stat t] returns the {!Stat.t} record associated with [t]. *)

val size : ro -> Optint.Int63.t
(** [size t] returns the size of [t]. *)

(** {2 Reading and writing} *)

val pread : ro -> file_offset:Optint.Int63.t -> Cstruct.t list -> int
(** [pread t ~file_offset bufs] performs a single read of [t] at [file_offset] into [bufs].

    It returns the number of bytes read, which may be less than the space in [bufs],
    even if more bytes are available. Use {!pread_exact} instead if you require
    the buffer to be filled.

    To read at the current offset, use {!Flow.single_read} instead. *)

val pread_exact : ro -> file_offset:Optint.Int63.t -> Cstruct.t list -> unit
(** [pread_exact t ~file_offset bufs] reads from [t] into [bufs] until [bufs] is full.

    @raise End_of_file if the buffer could not be filled. *)

val pwrite_single : rw -> file_offset:Optint.Int63.t -> Cstruct.t list -> int
(** [pwrite_single t ~file_offset bufs] performs a single write operation, writing
    data from [bufs] to location [file_offset] in [t].

    It returns the number of bytes written, which may be less than the length of [bufs].
    In most cases, you will want to use {!pwrite_all} instead. *)

val pwrite_all : rw -> file_offset:Optint.Int63.t -> Cstruct.t list -> unit
(** [pwrite_all t ~file_offset bufs] writes all the data in [bufs] to location [file_offset] in [t]. *)

val seek : ro -> Optint.Int63.t -> [`Set | `Cur | `End] -> Optint.Int63.t
(** Set and/or get the current file position.

    Like {!Unix.lseek}. *)

val sync : rw -> unit
(** Flush file buffers to disk.

    Like {!Unix.fsync}. *)

val truncate : rw -> Optint.Int63.t -> unit
(** Set the length of a file.

    Like {!Unix.ftruncate}. *)

(** {2 Provider Interface} *)

module Pi : sig
  val ro : (module READ with type t = 'a) ->
    < read : (module READ with type t = 'a)
    ; source : (module Flow.SOURCE with type t = 'a)
    ; close : 'a -> unit
    >

  val rw : (module WRITE with type t = 'a) ->
    < read : (module READ with type t = 'a)
    ; source : (module Flow.SOURCE with type t = 'a)
    ; close : 'a -> unit
    ; write : (module WRITE with type t = 'a)
    ; sink : (module Flow.SINK with type t = 'a)
    >
end
