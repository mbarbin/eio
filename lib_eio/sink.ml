module type S = sig
  type t
  val single_write : t -> Cstruct.t list -> int
  val copy : t -> src:Source.t -> unit
end

type t = T : ('a * < sink : (module S with type t = 'a); ..>) -> t [@@unboxed]

let make (type a) (module X : S with type t = a) (t : a) =
  T (t, object method sink = (module X : S with type t = a) end)
