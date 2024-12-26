type shutdown_command = [ `Receive | `Send | `All ]

type 'a read_method = 'a Read_method.t = ..
type 't read_method += Read_source_buffer of ('t -> (Cstruct.t list -> int) -> unit)

let simple_copy (type src) ~single_write t ~src:((src, src_ops) : (src, _) Source.t) =
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

type cstruct_source = Cstruct_source.t

let cstruct_source =
  let ops = Source.make (module Cstruct_source) in
  fun data -> ops (Cstruct_source.create data)

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

type string_source = String_source.t

let string_source =
  let ops = Source.make (module String_source) in
  fun s -> ops (String_source.create s)

let single_read (type a) ((t, ops) : (a, _) Source.t) buf =
  let module X = (val ops#source) in
  let got = X.single_read t buf in
  assert (got > 0 && got <= Cstruct.length buf);
  got

let rec read_exact t buf =
  if Cstruct.length buf > 0 then (
    let got = single_read t buf in
    read_exact t (Cstruct.shift buf got)
  )

let single_write (type a) ((t, ops) : (a, _) Sink.t) bufs =
  let module X = (val ops#sink) in
  X.single_write t bufs

let write (type a) ((t, ops) : (a, _) Sink.t) bufs =
  let module X = (val ops#sink) in
  let rec aux = function
    | [] -> ()
    | bufs ->
      let wrote = X.single_write t bufs in
      aux (Cstruct.shiftv bufs wrote)
  in
  aux bufs

let copy_src (type a) src ((t, ops) : (a, _) Sink.t) =
  let module S = (val ops#sink) in
  S.copy t ~src

let copy src sink = copy_src src sink
let copy_string s = copy_src (string_source s)

module Buffer_sink = struct
  type t = Buffer.t

  let single_write t bufs =
    let old_length = Buffer.length t in
    List.iter (fun buf -> Buffer.add_bytes t (Cstruct.to_bytes buf)) bufs;
    Buffer.length t - old_length

  let copy t ~src = simple_copy ~single_write t ~src
end

type buffer_sink = Buffer_sink.t

let buffer_sink =
  let ops = Sink.make (module Buffer_sink) in
  fun b -> ops b

module Source = Source

module Sink = Sink

module type SOURCE = Source.S
module type SINK = Sink.S

module type SHUTDOWN = sig
  type t
  val shutdown : t -> shutdown_command -> unit
end

module type S = sig
  type t
  include Shutdownable.S with type t := t
  include Source.S with type t := t
  include Sink.S with type t := t
end

class type ['a] flow = object
  method source : (module Source.S with type t = 'a)
  method sink : (module Sink.S with type t = 'a)
  method shutdown : (module Shutdownable.S with type t = 'a)
  method resource_store : 'a Resource_store.t
end

type ('a, 'r) t =
  ('a *
   (< source : (module Source.S with type t = 'a)
    ; sink : (module Sink.S with type t = 'a)
    ; shutdown : (module Shutdownable.S with type t = 'a)
    ; resource_store : 'a Resource_store.t
    ; ..> as 'r))

type 'a t' = ('a, 'a flow) t

type r = T : 'a t' -> r [@@unboxed]

module Pi = struct
  let make (type t) (module X : S with type t = t) (t : t) =
    let resource_store = Resource_store.create () in
    (t, object
      method shutdown = (module X : Shutdownable.S with type t = t)
      method source = (module X : Source.S with type t = t)
      method sink = (module X : Sink.S with type t = t)
      method resource_store = resource_store
    end)

  let simple_copy = simple_copy
end

module Closable_source = struct
  class type ['a] closable_source = object
    method source : (module Source.S with type t = 'a)
    method close : 'a -> unit
    method resource_store : 'a Resource_store.t
  end

  type ('a, 'r) t =
    ('a *
     (< source : (module Source.S with type t = 'a)
      ; close : 'a -> unit
      ; resource_store : 'a Resource_store.t
      ; ..> as 'r))

  type 'a t' = ('a, 'a closable_source) t

  type r = T : 'a t' -> r [@@unboxed]
end

module Closable_sink = struct
  class type ['a] closable_sink = object
    method sink : (module Sink.S with type t = 'a)
    method close : 'a -> unit
    method resource_store : 'a Resource_store.t
  end

  type ('a, 'r) t =
    ('a *
     (< sink : (module Sink.S with type t = 'a)
      ; close : 'a -> unit
      ; resource_store : 'a Resource_store.t
      ; ..> as 'r))

  type 'a t' = ('a, 'a closable_sink) t

  type r = T : 'a t' -> r [@@unboxed]
end

type ('a, 'r) closable_source = ('a, 'r) Closable_source.t

type ('a, 'r) closable_sink =  ('a, 'r) Closable_sink.t

let close = Closable.close

let shutdown (type a) ((t, ops) : (a, _) t) cmd =
  let module X = (val ops#shutdown) in
  X.shutdown t cmd

type ('a, 'r) source = ('a, 'r) Source.t
type ('a, 'r) sink = ('a, 'r) Sink.t
