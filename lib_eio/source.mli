(** A readable flow provides a stream of bytes. *)

module type S = sig
  type t
  val read_methods : t Read_method.t list
  val single_read : t -> Cstruct.t -> int
end

class type ['a] source = object
  method source : (module S with type t = 'a)
  method resource_store : 'a Resource_store.t
end

type ('a, 'r) t =
  ('a *
   (< source : (module S with type t = 'a)
    ; resource_store : 'a Resource_store.t
    ; ..> as 'r))

type 'a t' = ('a, 'a source) t

type r = T : 'a t' -> r

val make : (module S with type t = 'a) -> 'a -> 'a t'
