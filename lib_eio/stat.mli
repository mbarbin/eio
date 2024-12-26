(** Portable file stats. *)

(** Traditional Unix permissions. *)
module Unix_perm : sig
  type t = int
  (** This is the same as {!Unix.file_perm}, but avoids a dependency on [Unix]. *)
end

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
