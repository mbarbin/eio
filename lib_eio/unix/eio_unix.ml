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
  type 'net base = <
    stdin  : Eio.Flow.source;
    stdout : Eio.Flow.sink;
    stderr : Eio.Flow.sink;
    net : Net.packed;
    domain_mgr : Eio.Domain_manager.t;
    process_mgr : Process.mgr;
    clock : float Eio.Time.clock_ty r;
    mono_clock : Eio.Time.Mono.ty r;
    fs : Eio.Path.t;
    cwd : Eio.Path.t;
    secure_random : Eio.Flow.source;
    debug : Eio.Debug.t;
    backend_id: string;
  >
end
