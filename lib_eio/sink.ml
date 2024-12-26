module type S = sig
  type t
  val single_write : t -> Cstruct.t list -> int
  val copy : t -> src:_ Source.t -> unit
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

let make (type a) (module X : S with type t = a) (t : a) =
  let resource_store = Resource_store.create () in
  (t, object
     method sink = (module X : S with type t = a)
     method resource_store = resource_store
   end)
