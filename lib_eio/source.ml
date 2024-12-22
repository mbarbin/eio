module type S = sig
  type t
  val read_methods : t Read_method.t list
  val single_read : t -> Cstruct.t -> int
end

type t = T : ('a * < source : (module S with type t = 'a); ..>) -> t [@@unboxed]

let make (type a) (module X : S with type t = a) (t : a) =
  T (t, object method source = (module X : S with type t = a) end)
