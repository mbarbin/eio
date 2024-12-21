type t =
  | T :
      ('a *
       < listening_socket : (module Eio.Net.Listening_socket.S with type t = 'a)
       ; close : 'a -> unit
       ; fd : 'a -> Fd.t
       ; ..>)
      -> t [@@unboxed]

module Cast = struct
  let as_generic_listening_socket (T t) = Eio.Net.Listening_socket.T t
end

let close (T (a, ops)) = ops#close a
let fd (T (a, ops)) = ops#fd a

module type S = sig
  include Eio.Net.Listening_socket.S

  val fd : t -> Fd.t
end

module Pi = struct
  let make (type a) (module X : S with type t = a) (t : a) =
    T
      (t, object
         method listening_socket = (module X : Eio.Net.Listening_socket.S with type t = a)
         method close = X.close
         method fd = X.fd
       end)
end
