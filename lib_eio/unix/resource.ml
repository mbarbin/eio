(* CR mbarbin: By the time we are done with the refactoring, this
   module can probably be deleted as I expect it won't be used as is
   but rather inlined in fd method where needed. *)
type 'a t = ([> `Unix_fd] as 'a) Eio.Resource.t

type ('t, _, _) Eio.Resource.pi += T : ('t, 't -> Fd.t, [> `Unix_fd]) Eio.Resource.pi
let fd (Eio.Resource.T (t, ops)) = Eio.Resource.get ops T t

let fd_opt (Eio.Resource.T (t, ops)) =
  match Eio.Resource.get_opt ops T with
  | Some f -> Some (f t)
  | None -> None
