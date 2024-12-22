module type S = sig
  type t

  val accept : t -> sw:Eio.Switch.t -> Stream_socket.t * Eio.Net.Sockaddr.stream
  val close : t -> unit
  val listening_addr : t -> Eio.Net.Sockaddr.stream
  val fd : t -> Fd.t
end

type t =
  | T :
      ('a *
       < listening_socket : (module Eio.Net.Listening_socket.S with type t = 'a)
       ; unix_listening_socket : (module S with type t = 'a)
       ; close : 'a -> unit
       ; fd : 'a -> Fd.t
       ; resource_store : 'a Eio.Resource_store.t
       ; ..>)
      -> t [@@unboxed]

module Cast = struct
  let as_generic_listening_socket (T t) = Eio.Net.Listening_socket.T t
end

let close (T (a, ops)) = ops#close a
let fd (T (a, ops)) = ops#fd a

module Pi = struct
  (* CR mbarbin: Settle on the staged scheme consistently. *)
  let make (type a) (module X : S with type t = a) =
    let resource_store = Eio.Resource_store.create () in
    Eio.Resource_store.set resource_store ~key:Fd.key.key ~data:X.fd;
    let module Generic = struct
      include X

      let accept t ~sw =
        let stream_socket, stream = X.accept t ~sw in
        Stream_socket.Cast.as_generic_stream_socket stream_socket, stream
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
    fun (t : a) -> T (t, ops)
end
