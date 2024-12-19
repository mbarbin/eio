(** This extends the {!Eio.Process} API with more control over file-descriptors. *)

open Eio.Std

(** {2 Types}

    These extend the types in {!Eio.Process} with support for file descriptors. *)

type t = Process : ('a * ('a, [> `Generic | `Unix ], _) Eio.Process.process_ty) -> t [@@unboxed]
type process := t

module Process : sig
  val to_generic : t -> Eio.Process.t
end

module type MGR_unix = sig
  include Eio.Process.MGR

  val spawn_unix :
    t ->
    sw:Switch.t ->
    ?cwd:Eio.Path.t ->
    env:string array ->
    fds:(int * Fd.t * Fork_action.blocking) list ->
    executable:string ->
    string list ->
    process
end

type ('t, 'tag, 'row) mgr_ty =
  < mgr : (module Eio.Process.MGR with type t = 't and type tag = 'tag)
  ; mgr_unix :  (module MGR_unix with type t = 't and type tag = 'tag)
  ; .. > as 'row

type mgr = Mgr : ('a * ('a, [> `Generic | `Unix ], _) mgr_ty) -> mgr [@@unboxed]

module Mgr : sig
  val to_generic : mgr -> Eio.Process.mgr
end

module Pi : sig
  val mgr_unix :
    (module MGR_unix with type t = 't and type tag = 'tag) ->
    < mgr : (module Eio.Process.MGR with type t = 't and type tag = 'tag)
    ; mgr_unix : (module MGR_unix with type t = 't and type tag = 'tag)
    >
end

module Make_mgr (X : sig
  type t

  val spawn_unix :
    t ->
    sw:Switch.t ->
    ?cwd:Eio.Path.t ->
    env:string array ->
    fds:(int * Fd.t * Fork_action.blocking) list ->
    executable:string ->
    string list ->
    process
end) : MGR_unix with type t = X.t and type tag = [`Generic | `Unix]

val spawn_unix :
    sw:Switch.t ->
    mgr ->
    ?cwd:Eio.Path.t ->
    fds:(int * Fd.t * Fork_action.blocking) list ->
    ?env:string array ->
    ?executable:string ->
    string list ->
    process
(** [spawn_unix ~sw mgr ~fds args] spawns a child process running the command [args].

    The arguments are as for {!Eio.Process.spawn},
    except that it takes a list of FD mappings for {!Fork_action.inherit_fds}
    directly, rather than just flows for the standard streams. *)

val sigchld : Eio.Condition.t
(** {b If} an Eio backend installs a SIGCHLD handler, the handler will broadcast on this condition.

    This allows non-Eio libraries (such as Lwt) to share its signal handler.

    Note: Not all backends install a handler (e.g. eio_linux uses process descriptors instead),
    so be sure to call {!install_sigchld_handler} if you need to use this. *)

val install_sigchld_handler : unit -> unit
(** [install_sigchld_handler ()] sets the signal handler for SIGCHLD to broadcast {!sigchld}. *)
