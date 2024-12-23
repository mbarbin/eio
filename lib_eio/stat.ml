module Unix_perm = struct
  type t = int
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

let pp_kind ppf = function
  | `Unknown -> Fmt.string ppf "unknown"
  | `Fifo -> Fmt.string ppf "fifo"
  | `Character_special -> Fmt.string ppf "character special file"
  | `Directory -> Fmt.string ppf "directory"
  | `Block_device -> Fmt.string ppf "block device"
  | `Regular_file -> Fmt.string ppf "regular file"
  | `Symbolic_link -> Fmt.string ppf "symbolic link"
  | `Socket -> Fmt.string ppf "socket"

type t = {
  dev : Int64.t;
  ino : Int64.t;
  kind : kind;
  perm : Unix_perm.t;
  nlink : Int64.t;
  uid : Int64.t;
  gid : Int64.t;
  rdev : Int64.t;
  size : Optint.Int63.t;
  atime : float;
  mtime : float;
  ctime : float;
}

let pp ppf t =
  Fmt.record [
    Fmt.field "dev" (fun t -> t.dev) Fmt.int64;
    Fmt.field "ino" (fun t -> t.ino) Fmt.int64;
    Fmt.field "kind" (fun t -> t.kind) pp_kind;
    Fmt.field "perm" (fun t -> t.perm) (fun ppf i -> Fmt.pf ppf "0o%o" i);
    Fmt.field "nlink" (fun t -> t.nlink) Fmt.int64;
    Fmt.field "uid" (fun t -> t.uid) Fmt.int64;
    Fmt.field "gid" (fun t -> t.gid) Fmt.int64;
    Fmt.field "rdev" (fun t -> t.rdev) Fmt.int64;
    Fmt.field "size" (fun t -> t.size) Optint.Int63.pp;
    Fmt.field "atime" (fun t -> t.atime) Fmt.float;
    Fmt.field "mtime" (fun t -> t.mtime) Fmt.float;
    Fmt.field "ctime" (fun t -> t.ctime) Fmt.float;
  ] ppf t
