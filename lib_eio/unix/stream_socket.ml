module type S = sig
  include Eio.Net.Stream_socket.S

  val send_msg : t -> fds:Fd.t list -> Cstruct.t list -> int
  val recv_msg_with_fds : t -> sw:Eio.Switch.t -> max_fds:int -> Cstruct.t list -> int * Fd.t list
  val fd : t -> Fd.t
end

class type ['a] stream_socket = object
  method shutdown : (module Eio.Flow.SHUTDOWN with type t = 'a)
  method source : (module Eio.Flow.SOURCE with type t = 'a)
  method sink : (module Eio.Flow.SINK with type t = 'a)
  method close : 'a -> unit
  method fd : 'a -> Fd.t
  method stream_socket : (module S with type t = 'a)
  method resource_store : 'a Eio.Resource_store.t
end

type ('a, 'r) t =
  ('a *
   (< shutdown : (module Eio.Flow.SHUTDOWN with type t = 'a)
    ; source : (module Eio.Flow.SOURCE with type t = 'a)
    ; sink : (module Eio.Flow.SINK with type t = 'a)
    ; close : 'a -> unit
    ; fd : 'a -> Fd.t
    ; stream_socket : (module S with type t = 'a)
    ; resource_store : 'a Eio.Resource_store.t
    ; .. > as 'r))

type 'a t' = ('a, 'a stream_socket) t

type r = T : 'a t' -> r

(* {[
     module Cast = struct
       let as_source (T t) = Eio.Flow.Source.T t
       let as_sink (T t) = Eio.Flow.Sink.T t
       let as_two_way (T t) = Eio.Flow.T t
       let as_unix_source (T t) = Source.T t
       let as_unix_sink (T t) = Sink.T t
       let as_generic (T t) = Eio.Net.Stream_socket.T t
     end
   ]} *)

let make (type t) (module X : S with type t = t) (t : t) =
  let resource_store = Eio.Resource_store.create () in
  Eio.Resource_store.set resource_store ~key:Fd.key.key ~data:X.fd;
  (t, object
     method close = X.close
     method shutdown = (module X : Eio.Flow.SHUTDOWN with type t = t)
     method source = (module X : Eio.Flow.SOURCE with type t = t)
     method sink = (module X : Eio.Flow.SINK with type t = t)
     method fd = X.fd
     method stream_socket = (module X : S with type t = t)
     method resource_store = resource_store
   end)

let close (type a) ((a, ops) : (a, _) t) = ops#close a
let fd (type a) ((a, ops) : (a, _) t) = ops#fd a
