type 'a t
(** An ['a t] is a promise for a value of type ['a]. *)

type 'a u
(** An ['a u] is a resolver for a promise of type ['a]. *)

val create : ?label:string -> unit -> 'a t * 'a u
(** [create ()] is a fresh promise/resolver pair.
    The promise is initially unresolved. *)

val await : 'a t -> 'a
(** [await t] blocks until [t] is resolved.
    If [t] is already resolved then this returns immediately.
    If [t] is broken, it raises the exception. *)

val await_result : 'a t -> ('a, exn) result
(** [await_result t] is like [await t], but returns [Error ex] if [t] is broken
    instead of raising an exception. *)

val fulfill : 'a u -> 'a -> unit
(** [fulfill u v] successfully resolves [u]'s promise with the value [v].
    Any threads waiting for the result will be added to the run queue. *)

val break : 'a u -> exn -> unit
(** [break u ex] resolves [u]'s promise with the exception [ex].
    Any threads waiting for the result will be added to the run queue. *)

val resolve : 'a t -> ('a, exn) result -> unit
(** [resolve t (Ok x)] is [fulfill t x] and
    [resolve t (Error ex)] is [break t ex]. *)

val fulfilled : 'a -> 'a t
(** [fulfilled x] is a promise that is already fulfulled with result [x]. *)

val broken : exn -> 'a t
(** [broken x] is a promise that is already broken with exception [ex]. *)

type 'a waiters

type 'a state =
  | Unresolved of 'a waiters
  | Fulfilled of 'a
  | Broken of exn

val state : 'a t -> 'a state

val is_resolved : 'a t -> bool
(** [is_resolved t] is [true] iff [state t] is [Fulfilled] or [Broken]. *)

(** {2 Provider API} *)

val add_waiter : 'a waiters -> (('a, exn) result -> unit) -> unit

effect Await : Ctf.id * 'a waiters -> 'a
(** Performed when the user calls [await] on an unresolved promise.
    The handler should add itself to the list of waiters and block until its callback is invoked.
    The ID is used for tracing. *)

val id : _ t -> Ctf.id