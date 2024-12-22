module type S = sig
  type t
  val single_write : t -> Cstruct.t list -> int
  val copy : t -> src:Source.t -> unit
end

type t =
  | T :
      ('a *
       < sink : (module S with type t = 'a)
       ; resource_store : 'a Resource_store.t
       ; ..>)
      -> t [@@unboxed]

let make (type a) (module X : S with type t = a) (t : a) =
  let resource_store = Resource_store.create () in
  T (t, object
       method sink = (module X : S with type t = a)
       method resource_store = resource_store
     end)
