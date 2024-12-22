
(* CR mbarbin: Is the [close] op needed at sites using this type? *)

(* CR mbarbin: With the addition of [resource_store] the updated plan is to
   remove this type entirely, and unify its use with the regular [Eio] +
   resource_store modules. *)

type t =
  | T :
      ('a *
       < source : (module Eio.Flow.SOURCE with type t = 'a)
       ; close : ('a -> unit) option
       ; fd : ('a -> Fd.t) option
       ; resource_store : 'a Eio.Resource_store.t
       ; ..>)
      -> t [@@unboxed]

module Cast = struct
  let as_generic (T (a, ops)) = Eio.Flow.Source.T (a, ops)
end

let of_generic (Eio.Flow.Source.T (a, ops)) =
  let source = ops#source in
  let resource_store = ops#resource_store in
  let fd = Eio.Resource_store.find resource_store ~key:Fd.key in
  T
    (a,
     object
       method source = source
       method close = None
       method fd = fd
       method resource_store = resource_store
     end)

let fd (T (a, ops)) =
  match ops#fd with
  | None -> None
  | Some fd -> Some (fd a)