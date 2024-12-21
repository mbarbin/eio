
(* CR mbarbin: Is the [close] op needed at sites using this type? *)

type t =
  | T :
      ('a *
       < source : (module Eio.Flow.SOURCE with type t = 'a)
       ; close : ('a -> unit) option
       ; fd : ('a -> Fd.t) option
       ; ..>)
      -> t [@@unboxed]

module Cast = struct
      let to_generic (T (a, ops)) = Eio.Flow.Source (a, ops)
end
let of_generic (Eio.Flow.Source (a, ops)) =
  T
    (a,
     object
       method source = ops#source
       method close = None
       method fd = None
     end)
