[@@@alert "-unstable"]

open Eio.Std

module Fd = Fd
module Private = Private
module Source = Source
module Sink = Sink
module Flow = Flow

let await_readable = Private.await_readable
let await_writable = Private.await_writable
let pipe = Private.pipe

type Eio.Exn.Backend.t += Unix_error of Unix.error * string * string
let () =
  Eio.Exn.Backend.register_pp (fun f -> function
      | Unix_error (code, name, arg) -> Fmt.pf f "Unix_error (%s, %S, %S)" (Unix.error_message code) name arg; true
      | _ -> false
    )

let sleep d =
  Eio.Time.Mono.sleep (Effect.perform Private.Get_monotonic_clock) d

let run_in_systhread = Thread_pool.run_in_systhread

module Ipaddr = Net.Ipaddr

module Process = Process
module Net = Net
module Cap = Cap

module Stdenv = struct
  type ('net, 'net_r) base_e = <
    stdin  : Eio.Flow.Source.r;
    stdout : Eio.Flow.Sink.r;
    stderr : Eio.Flow.Sink.r;
    net : ('net, 'net_r) Net.t;
    domain_mgr : Eio.Domain_manager.t;
    process_mgr : Process.mgr_r;
    clock : float Eio.Time.clock_ty r;
    mono_clock : Eio.Time.Mono.ty r;
    fs : Eio.Path.t;
    cwd : Eio.Path.t;
    secure_random : Eio.Flow.Source.r;
    debug : Eio.Debug.t;
    backend_id: string;
  >
  constraint 'net_r =
  ( < network : (module Eio.Net.NETWORK with type t = 'net)
    ; network_unix : (module Net.S with type t = 'net)
    ; .. > )

  type base = Env : _ base_e -> base
end
