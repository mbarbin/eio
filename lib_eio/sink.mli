(** A writeable flow accepts a stream of bytes. *)

module type S = sig
  type t
  val single_write : t -> Cstruct.t list -> int

  val copy : t -> src:_ Source.t -> unit
  (** [copy t ~src] allows for optimising copy operations.

        If you have no optimisations, you can use {!simple_copy} to implement this using {!single_write}. *)
end

type ('a, 'r) t =
  ('a *
   < sink : (module S with type t = 'a)
   ; resource_store : 'a Resource_store.t
   ; ..> as 'r)

val make : (module S with type t = 'a) -> 'a ->
  ('a, 'a *
   < sink : (module S with type t = 'a)
   ; resource_store : 'a Resource_store.t >
  ) t
