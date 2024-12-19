open Std

type exit_status = [
  | `Exited of int
  | `Signaled of int
]

type status = [ exit_status | `Stopped of int ]

let pp_status ppf = function
  | `Exited i -> Format.fprintf ppf "Exited (code %i)" i
  | `Signaled i -> Format.fprintf ppf "Exited (signal %a)" Fmt.Dump.signal i
  | `Stopped i -> Format.fprintf ppf "Stopped (signal %a)" Fmt.Dump.signal i

type error =
  | Executable_not_found of string
  | Child_error of exit_status

type Exn.err += E of error

let err e = Exn.create (E e)

let () =
  Exn.register_pp (fun f -> function
    | E e ->
      Fmt.string f "Process ";
      begin match e with
        | Executable_not_found e -> Fmt.pf f "Executable %S not found" e;
        | Child_error e -> Fmt.pf f "Child_error %a" pp_status e;
      end;
      true
    | _ -> false
  )

module type PROCESS = sig
  type t
  type tag

  val pid : t -> int
  val await : t -> exit_status
  val signal : t -> int -> unit
end

type ('t, 'tag, 'row) process_ty =
  < process : (module PROCESS with type t = 't and type tag = 'tag); .. > as 'row

type t = Process : ('a * ('a, [> `Generic], _) process_ty) -> t [@@unboxed]

type process = t

module type MGR = sig
  type tag
  type t

  val pipe :
    t ->
    sw:Switch.t ->
    [Flow.source_ty | Resource.close_ty] r * [Flow.sink_ty | Resource.close_ty] r

  val spawn :
    t ->
    sw:Switch.t ->
    ?cwd:Path.t ->
    ?stdin:Flow.source ->
    ?stdout:Flow.sink ->
    ?stderr:Flow.sink ->
    ?env:string array ->
    ?executable:string ->
    string list ->
    process
end

type ('t, 'tag, 'row) mgr_ty =
 < mgr : (module MGR with type t = 't and type tag = 'tag); .. > as 'row

type mgr = Mgr : ('a * ('a, [> `Generic], _) mgr_ty) -> mgr [@@unboxed]
module Pi = struct

  let process (type t tag) (module X : PROCESS with type t = t and type tag = tag) =
    object method process = (module X : PROCESS with type t = t and type tag = tag) end

  let mgr (type t tag) (module X : MGR with type t = t and type tag = tag) =
    object method mgr = (module X : MGR with type t = t and type tag = tag) end
end

let bad_char = function
  | ' ' | '"' | '\'' | '\\' -> true
  | c ->
    let c = Char.code c in
    c <= 32 || c >= 127

let pp_arg f x =
  if x = "" || String.exists bad_char x then Fmt.pf f "%S" x
  else Fmt.string f x

let pp_args = Fmt.hbox (Fmt.list ~sep:Fmt.sp pp_arg)

let await (Process (v, ops)) =
  let module X = (val ops#process) in
  X.await v

let await_exn ?(is_success = Int.equal 0) proc =
  match await proc with
  | `Exited code when is_success code -> ()
  | status -> raise (err (Child_error status))

let pid (t : t) =
  let (Process (v, ops)) = t in
  let module X = (val ops#process) in
  X.pid v

let signal (t : t) s =
  let (Process (v, ops)) = t in
  let module X = (val ops#process) in
  X.signal v s

let spawn ~sw (t : mgr) ?cwd ?stdin ?stdout ?stderr ?env ?executable args : t =
  let (Mgr (v, ops)) = t in
  let module X = (val ops#mgr) in
  X.spawn v ~sw
    ?cwd:(cwd :> Path.t option)
    ?env
    ?executable args
    ?stdin:(stdin :> Flow.source option)
    ?stdout:(stdout :> Flow.sink option)
    ?stderr:(stderr :> Flow.sink option)

let run t ?cwd ?stdin ?stdout ?stderr ?(is_success = Int.equal 0) ?env ?executable args =
  Switch.run ~name:"Process.run" @@ fun sw ->
  let child = spawn ~sw t ?cwd ?stdin ?stdout ?stderr ?env ?executable args in
  match await child with
  | `Exited code when is_success code -> ()
  | status ->
    let ex = err (Child_error status) in
    raise (Exn.add_context ex "running command: %a" pp_args args)

let pipe ~sw (Mgr (v, ops)) =
  let module X = (val ops#mgr) in
  X.pipe v ~sw

let parse_out (t : mgr) parse ?cwd ?stdin ?stderr ?is_success ?env ?executable args =
  Switch.run ~name:"Process.parse_out" @@ fun sw ->
  let r, w = pipe t ~sw in
  try
    let child = spawn ~sw t ?cwd ?stdin ~stdout:(Flow.Sink w) ?stderr ?env ?executable args in
    Flow.close w;
    let output = Buf_read.parse_exn parse (Flow.Source r) ~max_size:max_int in
    Flow.close r;
    await_exn ?is_success child;
    output
  with Exn.Io _ as ex ->
    let bt = Printexc.get_raw_backtrace () in
    Exn.reraise_with_context ex bt "running command: %a" pp_args args
