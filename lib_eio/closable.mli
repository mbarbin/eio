module type S = sig
  type t
  val close : t -> unit
end

class type ['a] closable = object
  method close : 'a -> unit
end

type ('a, 'r) t =
  ('a *
   (< close : 'a -> unit
    ; .. > as 'r))

type 'a t' = ('a, 'a closable) t

type packed = T : 'a t' -> packed

val close : _ t -> unit

val make : (module S with type t = 'a) -> 'a -> 'a t'
