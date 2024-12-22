(** A readable flow provides a stream of bytes. *)

module type S = sig
  type t
  val read_methods : t Read_method.t list
  val single_read : t -> Cstruct.t -> int
end

type t =
  | T :
      ('a *
       < source : (module S with type t = 'a)
       ; resource_store : 'a Resource_store.t
       ; ..>)
      -> t [@@unboxed]

val make : (module S with type t = 'a) -> 'a -> t
