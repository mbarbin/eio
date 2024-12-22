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

let read_of_fd ~sw ~default ~to_close = function
  | None -> default
  | Some (Eio.Flow.Source.T (f, ops) as source) ->
    match Eio.Resource_store.find ops#resource_store ~key:Fd.key.key with
    | Some fd -> fd f
    | None ->
      let r, w = Private.pipe sw in
      Fiber.fork ~sw (fun () ->
          Eio.Flow.copy source (Sink.Cast.as_generic w);
          Sink.close w
        );
      let r = Source.fd r in
      to_close := r :: !to_close;
      r

let write_of_fd ~sw ~default ~to_close = function
  | None -> default
  | Some (Eio.Flow.Sink.T (f, ops) as sink) ->
    match Eio.Resource_store.find ops#resource_store ~key:Fd.key.key with
    | Some fd -> fd f
    | None ->
      let r, w = Private.pipe sw in
      Fiber.fork ~sw (fun () ->
          Eio.Flow.copy (Source.Cast.as_generic r) sink;
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

type t =
  | Process :
      ('a *
       < process : (module Eio.Process.PROCESS with type t = 'a); .. >)
      -> t [@@unboxed]

type process = t

module Process = struct
  let as_generic (Process p) = Eio.Process.Process p
end

module type MGR_unix = sig
  type t

  val pipe :
    t ->
    sw:Switch.t ->
    Source.t * Sink.t

  val spawn :
    t ->
    sw:Switch.t ->
    ?cwd:Eio.Path.t ->
    ?stdin:Eio.Flow.Source.t ->
    ?stdout:Eio.Flow.Sink.t ->
    ?stderr:Eio.Flow.Sink.t ->
    ?env:string array ->
    ?executable:string ->
    string list ->
    Eio.Process.t
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

type mgr =
  | Mgr :
      ('a *
       < mgr : (module Eio.Process.MGR with type t = 'a)
       ; mgr_unix : (module MGR_unix with type t = 'a)
       ; .. >)
      -> mgr [@@unboxed]

module Mgr = struct
  let as_generic (Mgr (a, ops)) = Eio.Process.Mgr (a, ops)
end

module Pi = struct
  let process (type a) (module X : Eio.Process.PROCESS with type t = a) (t : a) =
    Process (t, object method process = (module X : Eio.Process.PROCESS with type t = a) end)

  let mgr_unix (type a) (module X : MGR_unix with type t = a) (t : a) =
    let module X_mgr = struct
      type t = X.t

      let spawn = X.spawn

      let pipe t ~sw =
        let (T r), (T w) = X.pipe t ~sw in
        (Eio.Flow.Closable.Closable_source r, Eio.Flow.Closable.Closable_sink w)
    end in
    Mgr (t, object
           method mgr = (module X_mgr : Eio.Process.MGR with type t = a)
           method mgr_unix = (module X : MGR_unix with type t = a)
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
    X.spawn_unix v ~sw ?cwd ~env ~fds ~executable args |> Process.as_generic

  let spawn_unix = X.spawn_unix
end

let spawn_unix ~sw (Mgr (v, ops) : mgr) ?cwd ~fds ?env ?executable args =
  let module X = (val ops#mgr_unix) in
  let executable = get_executable executable ~args in
  let env = get_env env in
  X.spawn_unix v ~sw ?cwd ~fds ~env ~executable args

let sigchld = Eio.Condition.create ()

let install_sigchld_handler () =
  Sys.(set_signal sigchld) (Signal_handle (fun (_:int) -> Eio.Condition.broadcast sigchld))
