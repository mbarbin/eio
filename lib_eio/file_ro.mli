module type S = sig
  include Flow.SOURCE

  val pread : t -> file_offset:Optint.Int63.t -> Cstruct.t list -> int
  val stat : t -> Stat.t
  val seek : t -> Optint.Int63.t -> [`Set | `Cur | `End] -> Optint.Int63.t
  val close : t -> unit
end

class type ['a] file_ro = object
  method read : (module S with type t = 'a)
  method source : (module Flow.SOURCE with type t = 'a)
  method close : 'a -> unit
  method resource_store : 'a Resource_store.t
end

type ('a, 'r) t =
  ('a *
   (< read : (module S with type t = 'a)
    ; source : (module Flow.SOURCE with type t = 'a)
    ; close : 'a -> unit
    ; resource_store : 'a Resource_store.t
    ; ..> as 'r))

type 'a t' = ('a, 'a file_ro) t

type r = T : 'a t' -> r [@@unboxed]

val make : (module S with type t = 'a) -> 'a -> 'a t'

val close : _ t -> unit
val find_store : _ t -> 'b Resource_store.accessor -> 'b option
