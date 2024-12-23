module type S = sig
  type t

  val accept : t -> sw:Eio.Switch.t -> Stream_socket.r * Eio.Net.Sockaddr.stream
  val close : t -> unit
  val listening_addr : t -> Eio.Net.Sockaddr.stream
  val fd : t -> Fd.t
end

class type ['a] listening_socket = object
  method listening_socket : (module Eio.Net.Listening_socket.S with type t = 'a)
  method unix_listening_socket : (module S with type t = 'a)
  method close : 'a -> unit
  method fd : 'a -> Fd.t
  method resource_store : 'a Eio.Resource_store.t
end

type ('a, 'r) t =
  ('a *
   (< listening_socket : (module Eio.Net.Listening_socket.S with type t = 'a)
    ; unix_listening_socket : (module S with type t = 'a)
    ; close : 'a -> unit
    ; fd : 'a -> Fd.t
    ; resource_store : 'a Eio.Resource_store.t
    ; ..> as 'r))

type 'a t' = ('a, 'a listening_socket) t

type r = T : 'a t' -> r [@@unboxed]

let make (type a) (module X : S with type t = a) =
  (* CR mbarbin: Settle on the staged scheme consistently. *)
  let resource_store = Eio.Resource_store.create () in
  Eio.Resource_store.set resource_store ~key:Fd.key.key ~data:X.fd;
  let module Generic : Eio.Net.Listening_socket.S with type t = a = struct
    include X

    let accept t ~sw =
      let (socket, stream) = X.accept t ~sw in
      Stream_socket.Cast.as_generic socket, stream
  end in
  let ops =
    object
      method listening_socket = (module Generic : Eio.Net.Listening_socket.S with type t = a)
      method unix_listening_socket = (module X : S with type t = a)
      method close = X.close
      method fd = X.fd
      method resource_store = resource_store
    end
  in
  fun (t : a) -> (t, ops)

let close (type a) ((a, ops) : (a, _) t) = ops#close a
let fd (type a) ((a, ops) : (a, _) t) = ops#fd a

module Cast = struct
  let as_generic (T (a, ops)) =
    Eio.Net.Listening_socket.T
      (a, (ops :> _ Eio.Net.Listening_socket.listening_socket))
end
