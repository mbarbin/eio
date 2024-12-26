(** This extends the {!Eio.Process} API with more control over file-descriptors. *)

open Eio.Std

(** {2 Types}

    These extend the types in {!Eio.Process} with support for file descriptors. *)

class type ['a] process_c = object
  method process : (module Eio.Process.PROCESS with type t = 'a)
  method resource_store : 'a Eio.Resource_store.t
end

type ('a, 'r) t =
  ('a *
   (< process : (module Eio.Process.PROCESS with type t = 'a)
    ; resource_store : 'a Eio.Resource_store.t
    ; .. > as 'r))
(** A process. *)

type 'a t' = ('a, 'a process_c) t

type r = T : 'a t' -> r [@@unboxed]

type process := r

module type MGR_unix = sig
  type t

  val pipe :
    t ->
    sw:Switch.t ->
    Source.r * Sink.r

  val spawn :
    t ->
    sw:Switch.t ->
    ?cwd:Eio.Path.t ->
    ?stdin:_ Eio.Flow.Source.t ->
    ?stdout:_ Eio.Flow.Sink.t ->
    ?stderr:_ Eio.Flow.Sink.t ->
    ?env:string array ->
    ?executable:string ->
    string list ->
    Eio.Process.r

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

class type ['a] mgr_c = object
  method mgr : (module Eio.Process.MGR with type t = 'a)
  method mgr_unix : (module MGR_unix with type t = 'a)
  method resource_store : 'a Eio.Resource_store.t
end

type ('a, 'r) mgr =
  ('a *
   (< mgr : (module Eio.Process.MGR with type t = 'a)
    ; mgr_unix : (module MGR_unix with type t = 'a)
    ; resource_store : 'a Eio.Resource_store.t
    ; .. > as 'r))

type 'a mgr' = ('a, 'a mgr_c) mgr

type mgr_r = Mgr : 'a mgr' -> mgr_r [@@unboxed]

module Cast : sig
  val as_generic_process : r -> Eio.Process.r
  val as_generic_mgr : mgr_r -> Eio.Process.mgr_r
end

module Pi : sig
  val process : (module Eio.Process.PROCESS with type t = 'a) -> 'a -> 'a t'
  val mgr_unix : (module MGR_unix with type t = 'a) -> 'a -> 'a mgr'
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
    _ mgr ->
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
