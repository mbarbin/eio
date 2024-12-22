
type shutdown_command = [
  | `Receive  (** Indicate that no more reads will be done *)
  | `Send     (** Indicate that no more writes will be done *)
  | `All      (** Indicate that no more reads or writes will be done *)
]

module type S = sig
  type t
  val shutdown : t -> shutdown_command -> unit
end

type t = T : ('a * < shutdown : (module S with type t = 'a); ..>) -> t [@@unboxed]

val make : (module S with type t = 'a) -> 'a -> t
