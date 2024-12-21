open Eio.Std

module Impl = struct
  type t = {
    label : string;
    on_listen : Eio.Net.Listening_socket.t Handler.t;
    on_connect : Eio.Net.Stream_socket.t Handler.t;
    on_datagram_socket : Eio.Net.Datagram_socket.t Handler.t;
    on_getaddrinfo : Eio.Net.Sockaddr.t list Handler.t;
    on_getnameinfo : (string * string) Handler.t;
  }

  let make label = {
    label;
    on_listen = Handler.make (`Raise (Failure "Mock listen handler not configured"));
    on_connect = Handler.make (`Raise (Failure "Mock connect handler not configured"));
    on_datagram_socket = Handler.make (`Raise (Failure "Mock datagram_socket handler not configured"));
    on_getaddrinfo = Handler.make (`Raise (Failure "Mock getaddrinfo handler not configured"));
    on_getnameinfo = Handler.make (`Raise (Failure "Mock getnameinfo handler not configured"));
  }

  let on_listen t = t.on_listen
  let on_connect t = t.on_connect
  let on_datagram_socket t = t.on_datagram_socket
  let on_getaddrinfo t = t.on_getaddrinfo
  let on_getnameinfo t = t.on_getnameinfo

  let listen t ~reuse_addr:_ ~reuse_port:_ ~backlog:_ ~sw addr =
    traceln "%s: listen on %a" t.label Eio.Net.Sockaddr.pp addr;
    let socket = Handler.run t.on_listen in
    Switch.on_release sw (fun () -> Eio.Net.Listening_socket.close socket);
    socket

  let connect t ~sw addr =
    traceln "%s: connect to %a" t.label Eio.Net.Sockaddr.pp addr;
    let socket = Handler.run t.on_connect in
    Switch.on_release sw (fun () -> Eio.Net.Stream_socket.close socket);
    socket

  let datagram_socket t ~reuse_addr:_ ~reuse_port:_ ~sw addr =
    (match addr with
     | #Eio.Net.Sockaddr.datagram as saddr -> traceln "%s: datagram_socket %a" t.label Eio.Net.Sockaddr.pp saddr
     | `UdpV4 -> traceln "%s: datagram_socket UDPv4" t.label
     | `UdpV6 -> traceln "%s: datagram_socket UDPv6" t.label
    );
    let socket = Handler.run t.on_datagram_socket in
    Switch.on_release sw (fun () -> Eio.Net.Datagram_socket.close socket);
    socket

  let getaddrinfo t ~service node =
    traceln "%s: getaddrinfo ~service:%s %s" t.label service node;
    Handler.run t.on_getaddrinfo

  let getnameinfo t sockaddr =
    traceln "%s: getnameinfo %a" t.label Eio.Net.Sockaddr.pp sockaddr;
    Handler.run t.on_getnameinfo
end

type t =
  | Network :
      ('a *
       < network : (module Eio.Net.NETWORK with type t = 'a)
       ; raw : 'a -> Impl.t
       ; ..>)
      -> t [@@unboxed]

let raw (Network (t, ops)) = ops#raw t

let make : string -> t =
  let ops =
    object
      method network = (module Impl : Eio.Net.NETWORK with type t = Impl.t)
      method raw = Fun.id
    end
  in
  fun label -> Network (Impl.make label, ops)

let on_connect (t : t) actions =
  let t = raw t in
  Handler.seq t.on_connect (List.map (Action.map Fun.id) actions)

let on_listen (t : t) actions =
  let t = raw t in
  Handler.seq t.on_listen (List.map (Action.map Fun.id) actions)

let on_datagram_socket (t : t) actions =
  let t = raw t in
  Handler.seq t.on_datagram_socket (List.map (Action.map Fun.id) actions)

let on_getaddrinfo (t:t) actions = Handler.seq (raw t).on_getaddrinfo actions

let on_getnameinfo (t:t) actions = Handler.seq (raw t).on_getnameinfo actions

module Listening_socket_impl = struct
  type t = {
    label : string;
    listening_addr : Eio.Net.Sockaddr.stream;
    on_accept : (Flow.t * Eio.Net.Sockaddr.stream) Handler.t;
  }

  let make ?(listening_addr = `Tcp (Eio.Net.Ipaddr.V4.any, 0)) label =
    {
      label;
      listening_addr;
      on_accept = Handler.make (`Raise (Failure "Mock accept handler not configured"))
    }

  let on_accept t = t.on_accept

  let accept t ~sw =
      let socket, addr = Handler.run t.on_accept in
      Flow.attach_to_switch (socket : Flow.t) sw;
      traceln "%s: accepted connection from %a" t.label Eio.Net.Sockaddr.pp addr;
      Flow.as_stream_socket socket, addr

  let close t =
    traceln "%s: closed" t.label

  let listening_addr { listening_addr; _ } = listening_addr
end

type listening_socket =
  | Listening_socket :
      ('a *
       < listening_socket : (module Eio.Net.Listening_socket.S with type t = 'a)
       ; close : 'a -> unit
       ; raw : 'a -> Listening_socket_impl.t
       ; ..>)
      -> listening_socket [@@unboxed]

let raw_listening_socket (Listening_socket (t, ops)) = ops#raw t

let listening_socket ?listening_addr label : listening_socket =
  let ops =
    object
      method close = Listening_socket_impl.close
      method listening_socket = (module Listening_socket_impl : Eio.Net.Listening_socket.S with type t = Listening_socket_impl.t)
      method raw = Fun.id
    end
  in
  Listening_socket (Listening_socket_impl.make ?listening_addr label, ops)

let on_accept l actions =
  let r = raw_listening_socket l in
  let as_accept_pair x = (x :> Flow.t * Eio.Net.Sockaddr.stream) in
  Handler.seq r.on_accept (List.map (Action.map as_accept_pair) actions)
