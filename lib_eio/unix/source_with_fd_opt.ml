
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
  T
    (a,
     object
       method source = ops#source
       method close = None
       method fd = None
       method resource_store = ops#resource_store
     end)

let fd (T (a, ops)) =
  match ops#fd with
  | None -> None
  | Some fd -> Some (fd a)