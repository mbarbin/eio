(** A writeable flow accepts a stream of bytes. *)

module type S = sig
  type t
  val single_write : t -> Cstruct.t list -> int

  val copy : t -> src:Source.t -> unit
  (** [copy t ~src] allows for optimising copy operations.

        If you have no optimisations, you can use {!simple_copy} to implement this using {!single_write}. *)

end

type t = T : ('a * < sink : (module S with type t = 'a); ..>) -> t [@@unboxed]

val make : (module S with type t = 'a) -> 'a -> t
