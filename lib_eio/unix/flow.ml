module type S = sig
  include Eio.File.File_rw.S
  include Stream_socket.S with type t := t
end

class type ['a] flow = object
  method shutdown : (module Eio.Flow.SHUTDOWN with type t = 'a)
  method source : (module Eio.Flow.SOURCE with type t = 'a)
  method sink : (module Eio.Flow.SINK with type t = 'a)
  method close : 'a -> unit
  method read : (module Eio.File.File_ro.S with type t = 'a)
  method write : (module Eio.File.File_rw.S with type t = 'a)
  method fd : 'a -> Fd.t
  method stream_socket : (module Stream_socket.S with type t = 'a)
  method resource_store : 'a Eio.Resource_store.t
end

type ('a, 'r) t =
  ('a *
   (< shutdown : (module Eio.Flow.SHUTDOWN with type t = 'a)
    ; source : (module Eio.Flow.SOURCE with type t = 'a)
    ; sink : (module Eio.Flow.SINK with type t = 'a)
    ; close : 'a -> unit
    ; read : (module Eio.File.File_ro.S with type t = 'a)
    ; write : (module Eio.File.File_rw.S with type t = 'a)
    ; fd : 'a -> Fd.t
    ; stream_socket : (module Stream_socket.S with type t = 'a)
    ; resource_store : 'a Eio.Resource_store.t
    ; ..> as 'r))

type 'a t' = ('a, 'a flow) t

type r = T : 'a t' -> r

let make (type a) (module X : S with type t = a) (t : a) =
  let resource_store = Eio.Resource_store.create () in
  Eio.Resource_store.set resource_store ~key:Fd.key.key ~data:X.fd;
  (t, object
     method shutdown = (module X : Eio.Flow.SHUTDOWN with type t = a)
     method source = (module X : Eio.Flow.SOURCE with type t = a)
     method sink = (module X : Eio.Flow.SINK with type t = a)
     method close = X.close
     method read = (module X : Eio.File.File_ro.S with type t = a)
     method write = (module X : Eio.File.File_rw.S with type t = a)
     method fd = X.fd
     method stream_socket = (module X : Stream_socket.S with type t = a)
     method resource_store = resource_store
   end)

let close (type a) ((t, ops) : (a, _) t) = ops#close t
