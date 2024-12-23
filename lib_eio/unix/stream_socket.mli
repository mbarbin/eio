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

val make : (module S with type t = 'a) -> 'a -> 'a t'

(* CR mbarbin: I wish to expose casters so it is more discoverable what is possible.
   Look into this consistently:

   {[
     module Cast : sig
       val as_source : t -> Eio.Flow.source
       val as_sink : t -> Eio.Flow.sink
       val as_two_way : t -> Eio.Flow.t
       val as_unix_source : t -> Source.t
       val as_unix_sink : t -> Sink.t
       val as_generic : t -> Eio.Net.Stream_socket.t
     end
   ]}
*)

val close : _ t -> unit
val fd : _ t -> Fd.t
