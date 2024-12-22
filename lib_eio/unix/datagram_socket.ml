type t =
  | T :
      ('a *
       < shutdown : (module Eio.Flow.SHUTDOWN with type t = 'a)
       ; datagram_socket : (module Eio.Net.Datagram_socket.S with type t = 'a)
       ; close : 'a -> unit
       ; fd : 'a -> Fd.t
       ; resource_store : 'a Eio.Resource_store.t
       ; .. >)
      -> t [@@unboxed]

module Cast = struct
  let as_generic_datagram_socket (T t) = Eio.Net.Datagram_socket.T t
end

let close (T (a, ops)) = ops#close a
let fd (T (a, ops)) = ops#fd a

module type S = sig
  include Eio.Net.Datagram_socket.S

  val fd : t -> Fd.t
end

module Pi = struct
  let make (type a) (module X : S with type t = a) (t : a) =
    let resource_store = Eio.Resource_store.create () in
    T
      (t, object
         method shutdown = (module X : Eio.Flow.SHUTDOWN with type t = a)
         method datagram_socket = (module X : Eio.Net.Datagram_socket.S with type t = a)
         method close = X.close
         method fd = X.fd
         method resource_store = resource_store
       end)
end

