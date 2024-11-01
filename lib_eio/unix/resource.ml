type 'a t = ([> `Unix_fd] as 'a) Eio.Resource.t

module T : sig
  val pi : ('t, 't -> Fd.t, [> `Unix_fd]) Eio.Resource.pi
end = Eio.Resource.Pi.Create (struct
  type 't iface = 't -> Fd.t
end)

let fd (Eio.Resource.T (t, ops)) = Eio.Resource.get ops T.pi t

let fd_opt (Eio.Resource.T (t, ops)) =
  match Eio.Resource.get_opt ops T.pi with
  | Some f -> Some (f t)
  | None -> None
