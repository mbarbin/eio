module Ro = File_ro
module Rw = File_rw

module Unix_perm = Stat.Unix_perm
module Stat = Stat

type ('a, 'r) ro = ('a, 'r) Ro.t
type ('a, 'r) rw = ('a, 'r) Rw.t

let stat (type a) ((t, ops) : (a, _) ro) =
  let module X = (val ops#read) in
  X.stat t

let size t = (stat t).size

let pread (type a) ((t, ops) : (a, _) ro) ~file_offset bufs =
  let module X = (val ops#read) in
  let got = X.pread t ~file_offset bufs in
  assert (got > 0 && got <= Cstruct.lenv bufs);
  got

let pread_exact (type a) ((t, ops) : (a, _) ro) ~file_offset bufs =
  let module X = (val ops#read) in
  let rec aux ~file_offset bufs =
    if Cstruct.lenv bufs > 0 then (
      let got = X.pread t ~file_offset bufs in
      let file_offset = Optint.Int63.add file_offset (Optint.Int63.of_int got) in
      aux ~file_offset (Cstruct.shiftv bufs got)
    )
  in
  aux ~file_offset bufs

let pwrite_single (type a) ((t, ops) : (a, _) rw) ~file_offset bufs =
  let module X = (val ops#write) in
  let got = X.pwrite t ~file_offset bufs in
  assert (got > 0 && got <= Cstruct.lenv bufs);
  got

let pwrite_all (type a) ((t, ops) : (a, _) rw) ~file_offset bufs =
  let module X = (val ops#write) in
  let rec aux ~file_offset bufs =
    if Cstruct.lenv bufs > 0 then (
      let got = X.pwrite t ~file_offset bufs in
      let file_offset = Optint.Int63.add file_offset (Optint.Int63.of_int got) in
      aux ~file_offset (Cstruct.shiftv bufs got)
    )
  in
  aux ~file_offset bufs

let seek (type a) ((t, ops) : (a, _) ro) off cmd =
  let module X = (val ops#read) in
  X.seek t off cmd

let sync (type a) ((t, ops) : (a, _) rw) =
  let module X = (val ops#write) in
  X.sync t

let truncate (type a) ((t, ops) : (a, _) rw) len =
  let module X = (val ops#write) in
  X.truncate t len
