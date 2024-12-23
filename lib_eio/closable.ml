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

let close (type a) ((t, ops) : (a, _) t) = ops#close t

let make (type t) (module X : S with type t = t) (t : t) =
  (t, object
     method close = X.close
   end)
