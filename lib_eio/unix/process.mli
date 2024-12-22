(** This extends the {!Eio.Process} API with more control over file-descriptors. *)

open Eio.Std

(** {2 Types}

    These extend the types in {!Eio.Process} with support for file descriptors. *)

type t =
  | Process :
      ('a *
       < process : (module Eio.Process.PROCESS with type t = 'a); .. >)
      -> t [@@unboxed]

type process := t

module Process : sig
  val as_generic : t -> Eio.Process.t
end

module type MGR_unix = sig
  type t

  val pipe :
    t ->
    sw:Switch.t ->
    Source.t * Sink.t

  val spawn :
    t ->
    sw:Switch.t ->
    ?cwd:Eio.Path.t ->
    ?stdin:Eio.Flow.Source.t ->
    ?stdout:Eio.Flow.Sink.t ->
    ?stderr:Eio.Flow.Sink.t ->
    ?env:string array ->
    ?executable:string ->
    string list ->
    Eio.Process.t

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

type mgr =
  | Mgr :
      ('a *
       < mgr : (module Eio.Process.MGR with type t = 'a)
       ; mgr_unix : (module MGR_unix with type t = 'a)
       ; .. >)
      -> mgr [@@unboxed]

module Mgr : sig
  val as_generic : mgr -> Eio.Process.mgr
end

module Pi : sig
  val process : (module Eio.Process.PROCESS with type t = 'a) -> 'a -> process
  val mgr_unix :
    (module MGR_unix with type t = 'a) -> 'a -> mgr
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
end) : MGR_unix with type t = X.t

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
