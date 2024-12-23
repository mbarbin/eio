open Std

type connection_failure =
  | Refused of Exn.Backend.t
  | No_matching_addresses
  | Timeout

type error =
  | Connection_reset of Exn.Backend.t
  | Connection_failure of connection_failure

type Exn.err += E of error

let err e = Exn.create (E e)

let () =
  Exn.register_pp (fun f -> function
      | E e ->
        Fmt.string f "Net ";
        begin match e with
          | Connection_reset e -> Fmt.pf f "Connection_reset %a" Exn.Backend.pp e
          | Connection_failure Refused e -> Fmt.pf f "Connection_failure Refused %a" Exn.Backend.pp e
          | Connection_failure Timeout -> Fmt.pf f "Connection_failure Timeout"
          | Connection_failure No_matching_addresses -> Fmt.pf f "Connection_failure No_matching_addresses"
        end;
        true
      | _ -> false
    )

module Ipaddr = Ipaddr

module Sockaddr = Sockaddr

module Stream_socket = Stream_socket

module Listening_socket = Listening_socket

type 'a connection_handler = 'a Stream_socket.t' -> Sockaddr.stream -> unit

module Datagram_socket = Datagram_socket

module type NETWORK = sig
  type t

  val listen : t -> reuse_addr:bool -> reuse_port:bool -> backlog:int -> sw:Switch.t -> Sockaddr.stream -> Listening_socket.t
  val connect : t -> sw:Switch.t -> Sockaddr.stream -> _ Stream_socket.t
  val datagram_socket :
    t
    -> reuse_addr:bool
    -> reuse_port:bool
    -> sw:Switch.t
    -> [Sockaddr.datagram | `UdpV4 | `UdpV6]
    -> _ Datagram_socket.t

  val getaddrinfo : t -> service:string -> string -> Sockaddr.t list
  val getnameinfo : t -> Sockaddr.t -> (string * string)
end

type ('a, 'r) t =
  ('a *
   (< network : (module NETWORK with type t = 'a)
   ; ..
   > as 'r))

module Pi = struct
  let network (type t) (module X : NETWORK with type t = t) (t : t) =
    (t, object
      method network = (module X : NETWORK with type t = t)
    end)
end

let accept (type a) ~sw ((t, ops) : (a, _) Listening_socket.t) =
  let module X = (val ops#listening_socket) in
  X.accept t ~sw

let accept_fork ~sw (t : _ Listening_socket.t) ~on_error handle =
  let child_started = ref false in
  let (Stream_socket.T flow), addr = accept ~sw t in
  Fun.protect ~finally:(fun () -> if !child_started = false then Stream_socket.close flow)
    (fun () ->
       Fiber.fork ~sw (fun () ->
           match child_started := true; handle flow addr with
           | x -> Stream_socket.close flow; x
           | exception (Cancel.Cancelled _ as ex) ->
             Stream_socket.close flow;
             raise ex
           | exception ex ->
             Stream_socket.close flow;
             on_error (Exn.add_context ex "handling connection from %a" Sockaddr.pp addr)
         )
    )

let listening_addr (Listening_socket.T (t, ops)) =
  let module X = (val ops#listening_socket) in
  X.listening_addr t

let send (Datagram_socket.T (t, ops)) ?dst bufs =
  let module X = (val ops#datagram_socket) in
  X.send t ?dst bufs

let recv (Datagram_socket.T (t, ops)) buf =
  let module X = (val ops#datagram_socket) in
  X.recv t buf

let listen (type a) ?(reuse_addr=false) ?(reuse_port=false) ~backlog ~sw ((t, ops) : (a, _) t) =
  let module X = (val ops#network) in
  X.listen t ~reuse_addr ~reuse_port ~backlog ~sw

let connect (type a) ~sw ((t, ops) : (a, _) t) addr =
  let module X = (val ops#network) in
  try X.connect t ~sw addr
  with Exn.Io _ as ex ->
    let bt = Printexc.get_raw_backtrace () in
    Exn.reraise_with_context ex bt "connecting to %a" Sockaddr.pp addr

let datagram_socket (type a) ?(reuse_addr=false) ?(reuse_port=false) ~sw ((t, ops) : (a, _) t) addr =
  let module X = (val ops#network) in
  let addr = (addr :> [Sockaddr.datagram | `UdpV4 | `UdpV6]) in
  X.datagram_socket t ~reuse_addr ~reuse_port ~sw addr

let getaddrinfo (type a) ?(service="") ((t, ops) : (a, _) t) hostname =
  let module X = (val ops#network) in
  X.getaddrinfo t ~service hostname

let getaddrinfo_stream ?service t hostname =
  getaddrinfo ?service t hostname
  |> List.filter_map (function
      | #Sockaddr.stream as x -> Some x
      | _ -> None
    )

let getaddrinfo_datagram ?service t hostname =
  getaddrinfo ?service t hostname
  |> List.filter_map (function
      | #Sockaddr.datagram as x -> Some x
      | _ -> None
    )

let getnameinfo (type a) ((t, ops) : (a, _) t) sockaddr =
  let module X = (val ops#network) in
  X.getnameinfo t sockaddr

let close = Resource.close

let with_tcp_connect ?(timeout=Time.Timeout.none) ~host ~service t f =
  Switch.run ~name:"with_tcp_connect" @@ fun sw ->
  match
    let rec aux = function
      | [] -> raise @@ err (Connection_failure No_matching_addresses)
      | addr :: addrs ->
        try Time.Timeout.run_exn timeout (fun () -> connect ~sw t addr) with
        | Time.Timeout | Exn.Io _ when addrs <> [] ->
          aux addrs
        | Time.Timeout ->
          raise @@ err (Connection_failure Timeout)
    in
    getaddrinfo_stream ~service t host
    |> List.filter_map (function
        | `Tcp _ as x -> Some x
        | `Unix _ -> None
      )
    |> aux
  with
  | conn -> f conn
  | exception (Exn.Io _ as ex) ->
    let bt = Printexc.get_raw_backtrace () in
    Exn.reraise_with_context ex bt "connecting to %S:%s" host service

(* Run a server loop in a single domain. *)
let run_server_loop ~sw ~connections ~on_error ~stop listening_socket connection_handler =
  let rec accept () =
    Semaphore.acquire connections;
    accept_fork ~sw ~on_error listening_socket (fun conn addr ->
        Fun.protect (fun () -> connection_handler conn addr)
            ~finally:(fun () -> Semaphore.release connections)
      );
    accept ()
  in
  match stop with
  | None -> accept ()
  | Some stop -> Fiber.first accept (fun () -> Promise.await stop)

let run_server ?(max_connections=Int.max_int) ?(additional_domains) ?stop ~on_error listening_socket connection_handler : 'a =
  if max_connections <= 0 then invalid_arg "max_connections";
  Switch.run ~name:"run_server" @@ fun sw ->
  let connections = Semaphore.make max_connections in
  let run_server_loop sw = run_server_loop ~sw ~connections ~on_error ~stop listening_socket connection_handler in
  additional_domains |> Option.iter (fun (domain_mgr, domains) ->
      if domains < 0 then invalid_arg "additional_domains";
      for _ = 1 to domains do
        Fiber.fork ~sw (fun () -> Domain_manager.run domain_mgr (fun () ->
            Switch.run ~name:"run_server" @@ fun sw ->
            ignore (run_server_loop sw : 'a)
          ))
      done;
    );
  run_server_loop sw
