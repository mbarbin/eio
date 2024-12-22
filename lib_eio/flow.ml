type shutdown_command = [ `Receive | `Send | `All ]

type 'a read_method = 'a Read_method.t = ..
type 't read_method += Read_source_buffer of ('t -> (Cstruct.t list -> int) -> unit)

module type Read_S = sig
  type source

  val single_read : source -> Cstruct.t -> int
  val read_exact : source -> Cstruct.t -> unit
end

module type Write_S = sig
  type source
  type sink

  val write : sink -> Cstruct.t list -> unit
  val single_write : sink -> Cstruct.t list -> int
  val copy : source -> sink -> unit
  val copy_string : string -> sink -> unit
end

let simple_copy (type src) ~single_write t ~src:(Source.T (src, src_ops)) =
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

let string_source : string -> Source.t =
  let ops = Source.make (module String_source) in
  fun s -> ops (String_source.create s)

module Make_read (X : sig type source val cast : source -> Source.t end) = struct
  let single_read source buf =
    let (Source.T (t, ops)) = X.cast source in
    let module X = (val ops#source) in
    let got = X.single_read t buf in
    assert (got > 0 && got <= Cstruct.length buf);
    got

  let rec read_exact t buf =
    if Cstruct.length buf > 0 then (
      let got = single_read t buf in
      read_exact t (Cstruct.shift buf got)
    )
end

module Make_write (X : sig
    type source
    type sink
    val cast_source : source -> Source.t
    val cast_sink : sink -> Sink.t
  end) = struct

  let single_write sink bufs =
    let (Sink.T (t, ops)) = X.cast_sink sink in
    let module X = (val ops#sink) in
    X.single_write t bufs

  let write sink bufs =
    let (Sink.T (t, ops)) = X.cast_sink sink in
    let module X = (val ops#sink) in
    let rec aux = function
      | [] -> ()
      | bufs ->
        let wrote = X.single_write t bufs in
        aux (Cstruct.shiftv bufs wrote)
    in
    aux bufs

  let copy_src src sink =
    let (Sink.T (t, ops)) = X.cast_sink sink in
    let module S = (val ops#sink) in
    S.copy t ~src

  let copy src sink = copy_src (X.cast_source src) sink
  let copy_string s = copy_src (string_source s)
end

module Buffer_sink = struct
  type t = Buffer.t

  let single_write t bufs =
    let old_length = Buffer.length t in
    List.iter (fun buf -> Buffer.add_bytes t (Cstruct.to_bytes buf)) bufs;
    Buffer.length t - old_length

  let copy t ~src = simple_copy ~single_write t ~src
end

let buffer_sink =
  let ops = Sink.make (module Buffer_sink) in
  fun b -> ops b

module Source = Source

include Make_read (struct
    type source = Source.t
    let cast source = source
  end)

module Sink = Sink

include Make_write (struct
    type source = Source.t
    type sink = Sink.t
    let cast_source source = source
    let cast_sink sink = sink
  end)

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

type t = T : ('a *
 < source : (module Source.S with type t = 'a)
 ; sink : (module Sink.S with type t = 'a)
 ; shutdown : (module Shutdownable.S with type t = 'a)
 ; resource_store : 'a Resource_store.t
 ; ..>) -> t [@@unboxed]

module Cast = struct
  let as_source (T t) = Source.T t
  let as_sink (T t) = Sink.T t
end

module Pi = struct
  let make (type t) (module X : S with type t = t) (t : t) =
    let resource_store = Resource_store.create () in
    T (t, object
      method shutdown = (module X : Shutdownable.S with type t = t)
      method source = (module X : Source.S with type t = t)
      method sink = (module X : Sink.S with type t = t)
      method resource_store = resource_store
    end)

  let simple_copy = simple_copy
end

module Two_way = struct
  include Make_read (struct
      type source = t
      let cast = Cast.as_source
    end)

  include Make_write (struct
      type source = t
      type sink = t
      let cast_source = Cast.as_source
      let cast_sink = Cast.as_sink
    end)
end

let close = Resource.close

module Closable = struct
  type closable_source =
    | Closable_source :
        ('a *
         < source : (module Source.S with type t = 'a)
         ; close : 'a -> unit
         ; resource_store : 'a Resource_store.t
         ; ..>)
        -> closable_source [@@unboxed]

  type closable_sink =
    | Closable_sink :
        ('a *
         < sink : (module Sink.S with type t = 'a)
         ; close : 'a -> unit
         ; resource_store : 'a Resource_store.t
         ; ..>)
        -> closable_sink  [@@unboxed]

  (* CR mbarbin: Adopt the [Cast] naming scheme. *)
  let source (Closable_source s) = Source.T s
  let sink (Closable_sink s) = Sink.T s
end

let close_source (Closable.Closable_source (a, ops)) = ops#close a

let close_sink (Closable.Closable_sink (a, ops)) = ops#close a

let shutdown (T (t, ops)) cmd =
  let module X = (val ops#shutdown) in
  X.shutdown t cmd

type source = Source.t
type sink = Sink.t
