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

let make (type a) (module X : S with type t = a) (t : a) =
  let resource_store = Resource_store.create () in
  T (t, object
       method source = (module X : S with type t = a)
       method resource_store = resource_store
     end)
