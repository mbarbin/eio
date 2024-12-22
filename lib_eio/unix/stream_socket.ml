type t =
  | T :
      ('a *
       < shutdown : (module Eio.Flow.SHUTDOWN with type t = 'a)
       ; source : (module Eio.Flow.SOURCE with type t = 'a)
       ; sink : (module Eio.Flow.SINK with type t = 'a)
       ; close : 'a -> unit
       ; fd : 'a -> Fd.t
       ; .. >)
      -> t [@@unboxed]

module Cast = struct
  let as_source (T t) = Eio.Flow.Source t
  let as_sink (T t) = Eio.Flow.Sink t
  let as_two_way (T t) = Eio.Flow.Two_way t
  let as_unix_source (T t) = Source.T t
  let as_unix_sink (T t) = Sink.T t
  let as_generic_stream_socket (T t) = Eio.Net.Stream_socket.T t
end

let close (T (a, ops)) = ops#close a
let fd (T (a, ops)) = ops#fd a

module type S = sig
  include Eio.Net.Stream_socket.S

  val send_msg : t -> fds:Fd.t list -> Cstruct.t list -> int
  val recv_msg_with_fds : t -> sw:Eio.Switch.t -> max_fds:int -> Cstruct.t list -> int * Fd.t list
  val fd : t -> Fd.t
end

module Pi = struct
  let make (type t) (module X : S with type t = t) (t : t) =
    T
      (t, object
         method close = X.close
         method shutdown = (module X : Eio.Flow.SHUTDOWN with type t = t)
         method source = (module X : Eio.Flow.SOURCE with type t = t)
         method sink = (module X : Eio.Flow.SINK with type t = t)
         method fd = X.fd
       end)
end
