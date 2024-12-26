(** A writeable flow accepts a stream of bytes. *)

module type S = sig
  type t
  val single_write : t -> Cstruct.t list -> int

  val copy : t -> src:_ Source.t -> unit
  (** [copy t ~src] allows for optimising copy operations.

        If you have no optimisations, you can use {!simple_copy} to implement this using {!single_write}. *)
end

class type ['a] sink = object
  method sink : (module S with type t = 'a)
  method resource_store : 'a Resource_store.t
end

type ('a, 'r) t =
  ('a *
   (< sink : (module S with type t = 'a)
    ; resource_store : 'a Resource_store.t
    ; ..> as 'r))

type 'a t' = ('a, 'a sink) t

type r = T : 'a t' -> r

val make : (module S with type t = 'a) -> 'a -> 'a t'
