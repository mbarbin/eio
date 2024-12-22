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

module Cast : sig
  val as_source : t -> Eio.Flow.source
  val as_sink : t -> Eio.Flow.sink
  val as_two_way : t -> Eio.Flow.t
  val as_unix_source : t -> Source.t
  val as_unix_sink : t -> Sink.t
  val as_generic_stream_socket : t -> Eio.Net.Stream_socket.t
end

val close : t -> unit
val fd : t -> Fd.t

module type S = sig
  include Eio.Net.Stream_socket.S

  val send_msg : t -> fds:Fd.t list -> Cstruct.t list -> int
  val recv_msg_with_fds : t -> sw:Eio.Switch.t -> max_fds:int -> Cstruct.t list -> int * Fd.t list
  val fd : t -> Fd.t
end

module Pi : sig
  val make : (module S with type t = 'a) -> 'a -> t
end
