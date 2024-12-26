module type S = sig
  include Eio.File.Rw.S
  include Stream_socket.S with type t := t
end

class type ['a] flow = object
  method shutdown : (module Eio.Flow.SHUTDOWN with type t = 'a)
  method source : (module Eio.Flow.SOURCE with type t = 'a)
  method sink : (module Eio.Flow.SINK with type t = 'a)
  method close : 'a -> unit
  method read : (module Eio.File.Ro.S with type t = 'a)
  method write : (module Eio.File.Rw.S with type t = 'a)
  method fd : 'a -> Fd.t
  method stream_socket : (module Stream_socket.S with type t = 'a)
  method resource_store : 'a Eio.Resource_store.t
end

type ('a, 'r) t =
  ('a *
   (< shutdown : (module Eio.Flow.SHUTDOWN with type t = 'a)
    ; source : (module Eio.Flow.SOURCE with type t = 'a)
    ; sink : (module Eio.Flow.SINK with type t = 'a)
    ; close : 'a -> unit
    ; read : (module Eio.File.Ro.S with type t = 'a)
    ; write : (module Eio.File.Rw.S with type t = 'a)
    ; fd : 'a -> Fd.t
    ; stream_socket : (module Stream_socket.S with type t = 'a)
    ; resource_store : 'a Eio.Resource_store.t
    ; ..> as 'r))

type 'a t' = ('a, 'a flow) t

type r = T : 'a t' -> r [@@unboxed]

val make : (module S with type t = 'a) -> 'a -> 'a t'

val close : _ t -> unit

module Cast : sig
  val as_generic_source : r -> Eio.Flow.Source.r
  val as_generic_sink : r -> Eio.Flow.Sink.r
  val as_unix_source : r -> Source.r
  val as_unix_sink : r -> Sink.r
  val as_file_ro : r -> Eio.File.Ro.r
  val as_file_rw : r -> Eio.File.Rw.r
  val as_unix_stream_socket : r -> Stream_socket.r
end
