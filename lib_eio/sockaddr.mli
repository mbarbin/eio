(** Network addresses. *)

type stream = [
  | `Unix of string
  | `Tcp of Ipaddr.v4v6 * int
]
(** Socket addresses that we can build a {! Flow.two_way} for i.e. stream-oriented
      protocols. *)

type datagram = [
  | `Udp of Ipaddr.v4v6 * int
  | `Unix of string
]
(** Socket addresses that are message-oriented. *)

type t = [ stream | datagram ]

val pp : Format.formatter -> [< t] -> unit
