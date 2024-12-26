open Eio.Std

let resolve_program name =
  if Filename.is_implicit name then (
    Sys.getenv_opt "PATH"
    |> Option.value ~default:"/bin:/usr/bin"
    |> String.split_on_char ':'
    |> List.find_map (fun dir ->
        let p = Filename.concat dir name in
        if Sys.file_exists p then Some p else None
      )
  ) else if Sys.file_exists name then (
    Some name
  ) else None

let read_of_fd (type a) ~sw ~default ~to_close = function
  | None -> default
  | Some (((f, ops) : (a, _) Eio.Flow.Source.t) as source) ->
    match Eio.Resource_store.find ops#resource_store ~key:Fd.key.key with
    | Some fd -> fd f
    | None ->
      let (Source.T r), (Sink.T w) = Private.pipe sw in
      Fiber.fork ~sw (fun () ->
          Eio.Flow.copy source w;
          Sink.close w
        );
      let r = Source.fd r in
      to_close := r :: !to_close;
      r

let write_of_fd (type a) ~sw ~default ~to_close = function
  | None -> default
  | Some (((f, ops) : (a, _) Eio.Flow.Sink.t) as sink) ->
    match Eio.Resource_store.find ops#resource_store ~key:Fd.key.key with
    | Some fd -> fd f
    | None ->
      let (Source.T r, Sink.T w) = Private.pipe sw in
      Fiber.fork ~sw (fun () ->
          Eio.Flow.copy r sink;
          Source.close r
        );
      let w = Sink.fd w in
      to_close := w :: !to_close;
      w

let with_close_list fn =
  let to_close = ref [] in
  let close () =
    List.iter Fd.close !to_close
  in
  match fn to_close with
  | x -> close (); x
  | exception ex ->
    let bt = Printexc.get_raw_backtrace () in
    close ();
    Printexc.raise_with_backtrace ex bt

let get_executable ~args = function
  | Some exe -> exe
  | None ->
    match args with
    | [] -> invalid_arg "Arguments list is empty and no executable given!"
    | (x :: _) ->
      match resolve_program x with
      | Some x -> x
      | None -> raise (Eio.Process.err (Executable_not_found x))

let get_env = function
  | Some e -> e
  | None -> Unix.environment ()

class type ['a] process_c = object
  method process : (module Eio.Process.PROCESS with type t = 'a)
  method resource_store : 'a Eio.Resource_store.t
end

type ('a, 'r) t =
  ('a *
   (< process : (module Eio.Process.PROCESS with type t = 'a)
    ; resource_store : 'a Eio.Resource_store.t
    ; .. > as 'r))
(** A process. *)

type 'a t' = ('a, 'a process_c) t

type r = T : 'a t' -> r [@@unboxed]

type process = r

module Process = struct
  let as_generic_process (T (t, ops)) =
    Eio.Process.T
      (t, (ops :> _ Eio.Process.process_c))
end

module type MGR_unix = sig
  type t

  val pipe :
    t ->
    sw:Switch.t ->
    Source.r * Sink.r

  val spawn :
    t ->
    sw:Switch.t ->
    ?cwd:Eio.Path.t ->
    ?stdin:_ Eio.Flow.Source.t ->
    ?stdout:_ Eio.Flow.Sink.t ->
    ?stderr:_ Eio.Flow.Sink.t ->
    ?env:string array ->
    ?executable:string ->
    string list ->
    Eio.Process.r
  val spawn_unix :
    t ->
    sw:Switch.t ->
    ?cwd:Eio.Path.t ->
    env:string array ->
    fds:(int * Fd.t * Fork_action.blocking) list ->
    executable:string ->
    string list ->
    process
end

class type ['a] mgr_c = object
  method mgr : (module Eio.Process.MGR with type t = 'a)
  method mgr_unix : (module MGR_unix with type t = 'a)
  method resource_store : 'a Eio.Resource_store.t
end

type ('a, 'r) mgr =
  ('a *
   (< mgr : (module Eio.Process.MGR with type t = 'a)
    ; mgr_unix : (module MGR_unix with type t = 'a)
    ; resource_store : 'a Eio.Resource_store.t
    ; .. > as 'r))

type 'a mgr' = ('a, 'a mgr_c) mgr

type mgr_r = Mgr : 'a mgr' -> mgr_r [@@unboxed]

module Pi = struct
  let process (type a) (module X : Eio.Process.PROCESS with type t = a) (t : a) =
    let resource_store = Eio.Resource_store.create () in
    (t, object
       method process = (module X : Eio.Process.PROCESS with type t = a)
       method resource_store = resource_store
     end)

  let mgr_unix (type a) (module X : MGR_unix with type t = a) (t : a) =
    let resource_store = Eio.Resource_store.create () in
    let module X_mgr : Eio.Process.MGR with type t = a = struct
      type t = X.t

      let spawn = X.spawn

      let pipe t ~sw =
        let (r, w) = X.pipe t ~sw in
        (Source.Cast.as_closable_generic r, Sink.Cast.as_closable_generic w)
    end in
    (t, object
      method mgr = (module X_mgr : Eio.Process.MGR with type t = a)
      method mgr_unix = (module X : MGR_unix with type t = a)
      method resource_store = resource_store
    end)
end

module Make_mgr (X : sig
  type t

  val spawn_unix :
    t ->
    sw:Switch.t ->
    ?cwd:Eio.Path.t ->
    env:string array ->
    fds:(int * Fd.t * Fork_action.blocking) list ->
    executable:string ->
    string list ->
    process
end) = struct
  type t = X.t

  let pipe _ ~sw =
    Private.pipe sw

  let spawn v ~sw ?cwd ?stdin ?stdout ?stderr ?env ?executable args =
    let executable = get_executable executable ~args in
    let env = get_env env in
    with_close_list @@ fun to_close ->
    let stdin_fd  = read_of_fd  ~sw stdin  ~default:Fd.stdin  ~to_close in
    let stdout_fd = write_of_fd ~sw stdout ~default:Fd.stdout ~to_close in
    let stderr_fd = write_of_fd ~sw stderr ~default:Fd.stderr ~to_close in
    let fds = [
      0, stdin_fd, `Blocking;
      1, stdout_fd, `Blocking;
      2, stderr_fd, `Blocking;
    ] in
    X.spawn_unix v ~sw ?cwd ~env ~fds ~executable args |> Process.as_generic_process

  let spawn_unix = X.spawn_unix
end

let spawn_unix (type a) ~sw ((v, ops) : (a, _) mgr) ?cwd ~fds ?env ?executable args =
  let module X = (val ops#mgr_unix) in
  let executable = get_executable executable ~args in
  let env = get_env env in
  X.spawn_unix v ~sw ?cwd ~fds ~env ~executable args

let sigchld = Eio.Condition.create ()

let install_sigchld_handler () =
  Sys.(set_signal sigchld) (Signal_handle (fun (_:int) -> Eio.Condition.broadcast sigchld))

module Cast = struct
  let as_generic_process = Process.as_generic_process
  let as_generic_mgr (Mgr (a, ops)) =
    Eio.Process.Mgr (a, (ops :> _ Eio.Process.mgr_c))
end
