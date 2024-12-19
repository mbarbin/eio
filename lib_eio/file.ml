module Unix_perm = struct
  type t = int
end

module Stat = struct
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

module Ro = struct
  let to_source (Ro r) = Flow.Source r
end

type rw = Rw : ('a *
  < read : (module READ with type t = 'a)
  ; source : (module Flow.SOURCE with type t = 'a)
  ; close : 'a -> unit
  ; write : (module WRITE with type t = 'a)
  ; sink : (module Flow.SINK with type t = 'a)
  ; .. >) -> rw [@@unboxed]

module Rw = struct
  let to_ro (Rw r) = Ro r
  let to_sink (Rw r) = Flow.Sink r
end

module Pi = struct

  let ro (type t) (module X : READ with type t = t) =
    object
      method source = (module X : Flow.SOURCE with type t = t)
      method read = (module X : READ with type t = t)
      method close = X.close
    end

  let rw (type t) (module X : WRITE with type t = t) =
    object
      method source = (module X : Flow.SOURCE with type t = t)
      method read = (module X : READ with type t = t)
      method close = X.close
      method sink = (module X : Flow.SINK with type t = t)
      method write = (module X : WRITE with type t = t)
    end
end

let stat (Ro (t, ops)) =
  let module X = (val ops#read) in
  X.stat t

let size t = (stat t).size

let pread (Ro (t, ops)) ~file_offset bufs =
  let module X = (val ops#read) in
  let got = X.pread t ~file_offset bufs in
  assert (got > 0 && got <= Cstruct.lenv bufs);
  got

let pread_exact (Ro (t, ops)) ~file_offset bufs =
  let module X = (val ops#read) in
  let rec aux ~file_offset bufs =
    if Cstruct.lenv bufs > 0 then (
      let got = X.pread t ~file_offset bufs in
      let file_offset = Optint.Int63.add file_offset (Optint.Int63.of_int got) in
      aux ~file_offset (Cstruct.shiftv bufs got)
    )
  in
  aux ~file_offset bufs

let pwrite_single (Rw (t, ops)) ~file_offset bufs =
  let module X = (val ops#write) in
  let got = X.pwrite t ~file_offset bufs in
  assert (got > 0 && got <= Cstruct.lenv bufs);
  got

let pwrite_all (Rw (t, ops)) ~file_offset bufs =
  let module X = (val ops#write) in
  let rec aux ~file_offset bufs =
    if Cstruct.lenv bufs > 0 then (
      let got = X.pwrite t ~file_offset bufs in
      let file_offset = Optint.Int63.add file_offset (Optint.Int63.of_int got) in
      aux ~file_offset (Cstruct.shiftv bufs got)
    )
  in
  aux ~file_offset bufs

let seek (Ro (t, ops)) off cmd =
  let module X = (val ops#read) in
  X.seek t off cmd

let sync (Rw (t, ops)) =
  let module X = (val ops#write) in
  X.sync t

let truncate (Rw (t, ops)) len =
  let module X = (val ops#write) in
  X.truncate t len
