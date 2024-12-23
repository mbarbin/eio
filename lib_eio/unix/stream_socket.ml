module type S = sig
  include Eio.Net.Stream_socket.S

  val send_msg : t -> fds:Fd.t list -> Cstruct.t list -> int
  val recv_msg_with_fds : t -> sw:Eio.Switch.t -> max_fds:int -> Cstruct.t list -> int * Fd.t list
  val fd : t -> Fd.t
end

type t =
  | T :
      ('a *
       < shutdown : (module Eio.Flow.SHUTDOWN with type t = 'a)
       ; source : (module Eio.Flow.SOURCE with type t = 'a)
       ; sink : (module Eio.Flow.SINK with type t = 'a)
       ; close : 'a -> unit
       ; fd : 'a -> Fd.t
       ; stream_socket : (module S with type t = 'a)
       ; resource_store : 'a Eio.Resource_store.t
       ; .. >)
      -> t [@@unboxed]

module Cast = struct
  let as_source (T t) = Eio.Flow.Source.T t
  let as_sink (T t) = Eio.Flow.Sink.T t
  let as_two_way (T t) = Eio.Flow.T t
  let as_unix_source (T t) = Source.T t
  let as_unix_sink (T t) = Sink.T t
  let as_generic (T t) = Eio.Net.Stream_socket.T t
end

let close (T (a, ops)) = ops#close a
let fd (T (a, ops)) = ops#fd a

module Pi = struct
  let make (type t) (module X : S with type t = t) (t : t) =
    let resource_store = Eio.Resource_store.create () in
    Eio.Resource_store.set resource_store ~key:Fd.key.key ~data:X.fd;
    T
      (t, object
         method close = X.close
         method shutdown = (module X : Eio.Flow.SHUTDOWN with type t = t)
         method source = (module X : Eio.Flow.SOURCE with type t = t)
         method sink = (module X : Eio.Flow.SINK with type t = t)
         method fd = X.fd
         method stream_socket = (module X : S with type t = t)
         method resource_store = resource_store
       end)
end
