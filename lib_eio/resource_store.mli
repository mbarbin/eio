(** An heterogeneous table to attach data to resources.

    This is experimental, and furnishes an alternate to resource containing
   optional fields. In the Object design, resource that may contain additional
   and optional fields have an extra fields of this type to store the extra
   values.

    The main purpose of this design at this time is to allow storing optional
   fds (and perhaps close) in an attempt to simplify the types of the structure
   involved. *)

type 'a t

type ('a, 'data) key

module Key (X : sig
    type 'a t
  end) : sig
  val key : ('a, 'a X.t) key
end

val create : unit -> _ t

val set : 'a t -> key:('a, 'data) key -> data:'data -> unit

val find : 'a t -> key:('a, 'data) key -> 'data option
