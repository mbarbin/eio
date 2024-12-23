type stream = [
  | `Unix of string
  | `Tcp of Ipaddr.v4v6 * int
]

type datagram = [
  | `Udp of Ipaddr.v4v6 * int
  | `Unix of string
]

type t = [ stream | datagram ]

let pp f = function
  | `Unix path ->
    Format.fprintf f "unix:%s" path
  | `Tcp (addr, port) ->
    Format.fprintf f "tcp:%a:%d" Ipaddr.pp_for_uri addr port
  | `Udp (addr, port) ->
    Format.fprintf f "udp:%a:%d" Ipaddr.pp_for_uri addr port
