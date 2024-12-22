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

module Cast = struct
  let as_file_ro (T t) = Eio.File.Ro t
  let as_file_rw (T t) = Eio.File.Rw t
  let as_source (T t) = Eio.Flow.Source.T t
  let as_sink (T t) = Eio.Flow.Sink.T t
  let as_unix_source (T t) = Source.T t
  let as_unix_sink (T t) = Sink.T t
  let as_stream_socket (T t) = Eio.Net.Stream_socket.T t
  let as_unix_stream_socket (T t) = Stream_socket.T t
end

module type S = sig
  include Eio.File.WRITE
  include Stream_socket.S with type t := t
end

module Pi = struct
  let make (type a) (module X : S with type t = a) (t : a) =
    let resource_store = Eio.Resource_store.create () in
    Eio.Resource_store.set resource_store ~key:Fd.key.key ~data:X.fd;
    T
      (t, object
         method shutdown = (module X : Eio.Flow.SHUTDOWN with type t = a)
         method source = (module X : Eio.Flow.SOURCE with type t = a)
         method sink = (module X : Eio.Flow.SINK with type t = a)
         method close = X.close
         method read = (module X : Eio.File.READ with type t = a)
         method write = (module X : Eio.File.WRITE with type t = a)
         method fd = X.fd
         method stream_socket = (module X : Stream_socket.S with type t = a)
         method resource_store = resource_store
       end)
end

