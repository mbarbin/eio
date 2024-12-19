open Std

module type MGR = sig
  type t
  val run : t -> (cancelled:exn Promise.t -> 'a) -> 'a
  val run_raw : t -> (unit -> 'a) -> 'a
end

type ('a, 'b) mgr = < mgr : (module MGR with type t = 'a); .. > as 'b
type t = Domain_mgr : ('a * ('a, _) mgr) -> t [@@unboxed]

module Pi = struct
  let mgr (type t) (module X : MGR with type t = t) : (t, _) mgr =
    object method mgr = (module X : MGR with type t = t) end
end

let run_raw (Domain_mgr (t, ops)) fn =
  let module X = (val ops#mgr) in
  X.run_raw t fn

let run (Domain_mgr (t, ops)) fn =
  let module X = (val ops#mgr) in
  X.run t @@ fun ~cancelled ->
  (* If the spawning fiber is cancelled, [cancelled] gets set to the exception. *)
  try
    Fiber.first
      (fun () ->
         match Promise.await cancelled with
         | Cancel.Cancelled ex -> raise ex    (* To avoid [Cancelled (Cancelled ex))] *)
         | ex -> raise ex (* Shouldn't happen *)
      )
      fn
  with ex ->
    match Promise.peek cancelled with
    | Some (Cancel.Cancelled ex2 as cex) when ex == ex2 ->
      (* We unwrapped the exception above to avoid [fn] seeing a double cancelled exception.
         But this means that the top-level reported the original exception,
         which isn't what we want. *)
      raise cex
    | _ -> raise ex
