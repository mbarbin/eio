type t =
  | T :
      ('a *
       < shutdown : (module Eio.Flow.SHUTDOWN with type t = 'a)
       ; source : (module Eio.Flow.SOURCE with type t = 'a)
       ; sink : (module Eio.Flow.SINK with type t = 'a)
       ; close : 'a -> unit
       ; read : (module Eio.File.READ with type t = 'a)
       ; write : (module Eio.File.WRITE with type t = 'a)
       ; fd : 'a -> Fd.t
       ; stream_socket : (module Stream_socket.S with type t = 'a)
       ; resource_store : 'a Eio.Resource_store.t
       ; ..>)
      -> t [@@unboxed]

module Cast : sig
  val as_file_ro : t -> Eio.File.ro
  val as_file_rw : t -> Eio.File.rw

  val as_source : t -> Eio.Flow.source
  val as_sink : t -> Eio.Flow.sink

  val as_unix_source : t -> Source.t
  val as_unix_sink : t -> Sink.t

  val as_stream_socket : t -> Eio.Net.Stream_socket.t
  val as_unix_stream_socket : t -> Stream_socket.t
end

module type S = sig
  include Eio.File.WRITE
  include Stream_socket.S with type t := t
end

module Pi : sig
  val make : (module S with type t = 'a) -> 'a -> t
end
