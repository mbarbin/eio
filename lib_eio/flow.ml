type shutdown_command = [ `Receive | `Send | `All ]

type 't read_method = ..
type 't read_method += Read_source_buffer of ('t -> (Cstruct.t list -> int) -> unit)

module type SOURCE = sig
  type t
  val read_methods : t read_method list
  val single_read : t -> Cstruct.t -> int
end

type source = Source : ('a * < source : (module SOURCE with type t = 'a); ..>) -> source [@@unboxed]

module type SINK = sig
  type t
  val single_write : t -> Cstruct.t list -> int
  val copy : t -> src:source -> unit
end

type sink = Sink : ('a * < sink : (module SINK with type t = 'a); ..>) -> sink [@@unboxed]

module type SHUTDOWN = sig
  type t
  val shutdown : t -> shutdown_command -> unit
end

type shutdown = Shutdown : ('a * < shutdown : (module SHUTDOWN with type t = 'a); ..>) -> shutdown [@@unboxed]

module type TWO_WAY = sig
  include SHUTDOWN
  include SOURCE with type t := t
  include SINK with type t := t
end

type two_way = Two_way : ('a *
 < source : (module SOURCE with type t = 'a)
 ; sink : (module SINK with type t = 'a)
 ; shutdown : (module SHUTDOWN with type t = 'a)
 ; ..>) -> two_way [@@unboxed]

module Pi = struct
  let source (type t) (module X : SOURCE with type t = t) (t : t) =
    Source (t, object method source = (module X : SOURCE with type t = t) end)

  let sink (type t) (module X : SINK with type t = t) (t : t) =
    Sink (t, object method sink = (module X : SINK with type t = t) end)

  let shutdown (type t) (module X : SHUTDOWN with type t = t) (t : t) =
    Shutdown (t, object method shutdown = (module X : SHUTDOWN with type t = t) end)

  let two_way (type t) (module X : TWO_WAY with type t = t) (t : t) =
    Two_way (t, object
      method shutdown = (module X : SHUTDOWN with type t = t)
      method source = (module X : SOURCE with type t = t)
      method sink = (module X : SINK with type t = t)
    end)

  let simple_copy (type src) ~single_write t ~src:(Source (src, src_ops)) =
    let rec write_all buf =
      if not (Cstruct.is_empty buf) then (
        let sent = single_write t [buf] in
        write_all (Cstruct.shift buf sent)
      )
    in
    let module Src = (val src_ops#source) in
    try
      let buf = Cstruct.create 4096 in
      while true do
        let got = Src.single_read src buf in
        write_all (Cstruct.sub buf 0 got)
      done
    with End_of_file -> ()
end

let close = Resource.close

module Closable = struct
  type closable_source = Closable_source : ('a * < source : (module SOURCE with type t = 'a); close : 'a -> unit; ..>) -> closable_source [@@unboxed]
  type closable_sink = Closable_sink : ('a * < sink : (module SINK with type t = 'a); close : 'a -> unit; ..>) -> closable_sink  [@@unboxed]

  let source (Closable_source s) = Source s
  let sink (Closable_sink s) = Sink s
end

let close_source (Closable.Closable_source (a, ops)) = ops#close a

let close_sink (Closable.Closable_sink (a, ops)) = ops#close a

let single_read (Source (t, ops)) buf =
  let module X = (val ops#source) in
  let got = X.single_read t buf in
  assert (got > 0 && got <= Cstruct.length buf);
  got

let rec read_exact t buf =
  if Cstruct.length buf > 0 then (
    let got = single_read t buf in
    read_exact t (Cstruct.shift buf got)
  )

module Cstruct_source = struct
  type t = Cstruct.t list ref

  let create data = ref data

  let read_source_buffer t fn =
    let rec aux () =
      match !t with
      | [] -> raise End_of_file
      | x :: xs when Cstruct.length x = 0 -> t := xs; aux ()
      | xs ->
        let n = fn xs in
        t := Cstruct.shiftv xs n
    in
    aux ()

  let read_methods =
    [ Read_source_buffer read_source_buffer ]

  let single_read t dst =
    let avail, src = Cstruct.fillv ~dst ~src:!t in
    if avail = 0 then raise End_of_file;
    t := src;
    avail

end

let cstruct_source data =
  Pi.source (module Cstruct_source) (Cstruct_source.create data)

module String_source = struct
  type t = {
    s : string;
    mutable offset : int;
  }

  let single_read t dst =
    if t.offset = String.length t.s then raise End_of_file;
    let len = min (Cstruct.length dst) (String.length t.s - t.offset) in
    Cstruct.blit_from_string t.s t.offset dst 0 len;
    t.offset <- t.offset + len;
    len

  let read_methods = []

  let create s = { s; offset = 0 }
end

let string_source s =
  Pi.source (module String_source) (String_source.create s)

let single_write (Sink (t, ops)) bufs =
  let module X = (val ops#sink) in
  X.single_write t bufs

let write (Sink (t, ops)) bufs =
  let module X = (val ops#sink) in
  let rec aux = function
    | [] -> ()
    | bufs ->
      let wrote = X.single_write t bufs in
      aux (Cstruct.shiftv bufs wrote)
  in
  aux bufs

let copy src (Sink (t, ops)) =
  let module X = (val ops#sink) in
  X.copy t ~src

let copy_string s = copy (string_source s)

module Buffer_sink = struct
  type t = Buffer.t

  let single_write t bufs =
    let old_length = Buffer.length t in
    List.iter (fun buf -> Buffer.add_bytes t (Cstruct.to_bytes buf)) bufs;
    Buffer.length t - old_length

  let copy t ~src = Pi.simple_copy ~single_write t ~src
end

let buffer_sink b =
  Pi.sink (module Buffer_sink) b

let shutdown (Two_way (t, ops)) cmd =
  let module X = (val ops#shutdown) in
  X.shutdown t cmd
