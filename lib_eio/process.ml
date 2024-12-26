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

  val pid : t -> int
  val await : t -> exit_status
  val signal : t -> int -> unit
end

class type ['a] process_c = object
  method process : (module PROCESS with type t = 'a)
  method resource_store : 'a Resource_store.t
end

type ('a, 'r) t =
  ('a *
   (< process : (module PROCESS with type t = 'a)
    ; resource_store : 'a Resource_store.t
    ; .. > as 'r))
(** A process. *)

type 'a t' = ('a, 'a process_c) t

type r = T : 'a t' -> r [@@unboxed]

type process = r

module type MGR = sig
  type t

  val pipe :
    t ->
    sw:Switch.t ->
    Flow.Closable_source.r * Flow.Closable_sink.r

  val spawn :
    t ->
    sw:Switch.t ->
    ?cwd:Path.t ->
    ?stdin:_ Flow.source ->
    ?stdout:_ Flow.sink ->
    ?stderr:_ Flow.sink ->
    ?env:string array ->
    ?executable:string ->
    string list ->
    process
end

class type ['a] mgr_c = object
  method mgr : (module MGR with type t = 'a)
  method resource_store : 'a Resource_store.t
end

type ('a, 'r) mgr =
  ('a *
   (< mgr : (module MGR with type t = 'a)
    ; resource_store : 'a Resource_store.t
    ; .. > as 'r))
(** A process manager capable of spawning new processes. *)

type 'a mgr' = ('a, 'a mgr_c) mgr

type mgr_r = Mgr : 'a mgr' -> mgr_r [@@unboxed]

module Pi = struct

  let process (type a) (module X : PROCESS with type t = a) (t : a) =
    let resource_store = Resource_store.create () in
    (t, object
       method process = (module X : PROCESS with type t = a)
       method resource_store = resource_store
     end)

  let mgr (type a) (module X : MGR with type t = a) (t : a) =
    let resource_store = Resource_store.create () in
    (t, object
       method mgr = (module X : MGR with type t = a)
       method resource_store = resource_store
     end)
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

let await (type a) ((v, ops) : (a, _) t) =
  let module X = (val ops#process) in
  X.await v

let await_exn ?(is_success = Int.equal 0) proc =
  match await proc with
  | `Exited code when is_success code -> ()
  | status -> raise (err (Child_error status))

let pid (type a) ((v, ops) : (a, _) t) =
  let module X = (val ops#process) in
  X.pid v

let signal (type a) ((v, ops) : (a, _) t) s =
  let module X = (val ops#process) in
  X.signal v s

let spawn (type a) ~sw ((v, ops) : (a, _) mgr) ?cwd ?stdin ?stdout ?stderr ?env ?executable args : r =
  let module X = (val ops#mgr) in
  X.spawn v ~sw
    ?cwd:(cwd :> Path.t option)
    ?env
    ?executable args
    ?stdin
    ?stdout
    ?stderr

let run t ?cwd ?stdin ?stdout ?stderr ?(is_success = Int.equal 0) ?env ?executable args =
  Switch.run ~name:"Process.run" @@ fun sw ->
  let T child = spawn ~sw t ?cwd ?stdin ?stdout ?stderr ?env ?executable args in
  match await child with
  | `Exited code when is_success code -> ()
  | status ->
    let ex = err (Child_error status) in
    raise (Exn.add_context ex "running command: %a" pp_args args)

let pipe (type a) ~sw ((v, ops) : (a, _) mgr) =
  let module X = (val ops#mgr) in
  X.pipe v ~sw

let parse_out (type a) (t : _ mgr) parse ?cwd ?stdin ?stderr ?is_success ?env ?executable args =
  Switch.run ~name:"Process.parse_out" @@ fun sw ->
  let (Flow.Closable_source.T r), (Flow.Closable_sink.T w) = pipe t ~sw in
  try
    let T child = spawn ~sw t ?cwd ?stdin ~stdout:w ?stderr ?env ?executable args in
    Flow.close w;
    let output = Buf_read.parse_exn parse r ~max_size:max_int in
    Flow.close r;
    await_exn ?is_success child;
    output
  with Exn.Io _ as ex ->
    let bt = Printexc.get_raw_backtrace () in
    Exn.reraise_with_context ex bt "running command: %a" pp_args args
