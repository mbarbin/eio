module type MGR = sig
  type t

  val run : t -> (cancelled:exn Promise.t -> 'a) -> 'a
  (** [run t fn] runs [fn ~cancelled] in a new domain.

      If the calling fiber is cancelled, [cancelled] becomes resolved to the {!Cancel.Cancelled} exception.
      [fn] should cancel itself in this case. *)

  val run_raw : t -> (unit -> 'a) -> 'a
end

type ('a, 'b) mgr = < mgr : (module MGR with type t = 'a); .. > as 'b
type t = Domain_mgr : ('a * ('a, _) mgr) -> t [@@unboxed]

val run : t -> (unit -> 'a) -> 'a
(** [run t f] runs [f ()] in a newly-created domain and returns the result.

    Other fibers in the calling domain can run in parallel with the new domain.

    Warning: [f] must only access thread-safe values from the calling domain,
    but this is not enforced by the type system.

    If the calling fiber is cancelled, this is propagated to the spawned domain. *)

val run_raw : t -> (unit -> 'a) -> 'a
(** [run_raw t f] is like {!run}, but does not run an event loop in the new domain,
    and so cannot perform IO, fork fibers, etc. *)

(** {2 Provider Interface} *)

module Pi : sig
  val mgr : (module MGR with type t = 'a) -> < mgr : (module MGR with type t = 'a) >
end
