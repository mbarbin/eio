module type S = sig
  type t

  val accept : t -> sw:Switch.t -> Stream_socket.r * Sockaddr.stream
  val close : t -> unit
  val listening_addr : t -> Sockaddr.stream
end

class type ['a] listening_socket = object
  method listening_socket : (module S with type t = 'a)
  method close : 'a -> unit
  method resource_store : 'a Resource_store.t
end

 type ('a, 'r) t =
   ('a *
    (< listening_socket : (module S with type t = 'a)
     ; close : 'a -> unit
     ; resource_store : 'a Resource_store.t
     ; ..> as 'r))

type 'a t' = ('a, 'a listening_socket) t

type r = T : 'a t' -> r

let make (type t) (module X : S with type t = t) (t : t) =
  let resource_store = Resource_store.create () in
  (t, object
     method close = X.close
     method listening_socket = (module X : S with type t = t)
     method resource_store = resource_store
   end)

(* CR mbarbin: Share this method somewhere, it is used consistently in
   this design. *)
let find_store (type a) ((t, ops) : (a, _) t) { Resource_store. key } =
  Resource_store.find ops#resource_store ~key
  |> Option.map (fun f -> f t)

let close (type a) (t : (a, _) t) = Closable.close t
