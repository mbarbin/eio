module type S = sig
  include Flow.SHUTDOWN
  val send : t -> ?dst:Sockaddr.datagram -> Cstruct.t list -> unit
  val recv : t -> Cstruct.t -> Sockaddr.datagram * int
  val close : t -> unit
end

class type ['a] datagram_socket = object
  method shutdown : (module Flow.SHUTDOWN with type t = 'a)
  method datagram_socket : (module S with type t = 'a)
  method close : 'a -> unit
  method resource_store : 'a Resource_store.t
end

type ('a, 'r) t =
  ('a *
   (< shutdown : (module Flow.SHUTDOWN with type t = 'a)
    ; datagram_socket : (module S with type t = 'a)
    ; close : 'a -> unit
    ; resource_store : 'a Resource_store.t
    ; .. > as 'r))

type 'a t' = ('a, 'a datagram_socket) t

type r = T : 'a t' -> r

let make (type t) (module X : S with type t = t) (t : t) =
  let resource_store = Resource_store.create () in
  (t, object
     method shutdown = (module X : Flow.SHUTDOWN with type t = t)
     method datagram_socket = (module X : S with type t = t)
     method close = X.close
     method resource_store = resource_store
   end)

(* CR mbarbin: Share this method somewhere, it is used consistently in
   this design. *)
let find_store (type a) ((t, ops) : (a, _) t) { Resource_store. key } =
  Resource_store.find ops#resource_store ~key
  |> Option.map (fun f -> f t)

let close (type a) (t : (a, _) t) = Closable.close t
