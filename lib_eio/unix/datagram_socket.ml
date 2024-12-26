module type S = sig
  include Eio.Net.Datagram_socket.S

  val fd : t -> Fd.t
end

class type ['a] datagram_socket = object
  method shutdown : (module Eio.Flow.SHUTDOWN with type t = 'a)
  method datagram_socket : (module Eio.Net.Datagram_socket.S with type t = 'a)
  method close : 'a -> unit
  method fd : 'a -> Fd.t
  method resource_store : 'a Eio.Resource_store.t
end

type ('a, 'r) t =
  ('a *
   (< shutdown : (module Eio.Flow.SHUTDOWN with type t = 'a)
    ; datagram_socket : (module Eio.Net.Datagram_socket.S with type t = 'a)
    ; close : 'a -> unit
    ; fd : 'a -> Fd.t
    ; resource_store : 'a Eio.Resource_store.t
    ; .. > as 'r))

type 'a t' = ('a, 'a datagram_socket) t

type r = T : 'a t' -> r [@@unboxed]

let make (type a) (module X : S with type t = a) (t : a) =
  let resource_store = Eio.Resource_store.create () in
  Eio.Resource_store.set resource_store ~key:Fd.key.key ~data:X.fd;
  (t, object
     method shutdown = (module X : Eio.Flow.SHUTDOWN with type t = a)
     method datagram_socket = (module X : Eio.Net.Datagram_socket.S with type t = a)
     method close = X.close
     method fd = X.fd
     method resource_store = resource_store
   end)

let close (type a) ((a, ops) : (a, _) t) = ops#close a
let fd (type a) ((a, ops) : (a, _) t) = ops#fd a

module Cast = struct
  let as_generic (T (a, ops)) =
    (* CR mbarbin: Is it possible to cast the tuple directly? (and
       avoid the allocation). *)
    Eio.Net.Datagram_socket.T
      (a, (ops :> _ Eio.Net.Datagram_socket.datagram_socket))
end
