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
  | Some (Eio.Flow.Source f) ->
    match Resource.fd_opt f with
    | Some fd -> fd
    | None ->
      let r, w = Private.pipe sw in
      Fiber.fork ~sw (fun () ->
          Eio.Flow.copy (Eio.Flow.Source f) (Eio.Flow.Sink w);
          Eio.Flow.close w
        );
      let r = Resource.fd r in
      to_close := r :: !to_close;
      r

let write_of_fd ~sw ~default ~to_close = function
  | None -> default
  | Some (Eio.Flow.Sink f) ->
    match Resource.fd_opt f with
    | Some fd -> fd
    | None ->
      let r, w = Private.pipe sw in
      Fiber.fork ~sw (fun () ->
          Eio.Flow.copy (Eio.Flow.Source r) (Eio.Flow.Sink f);
          Eio.Flow.close r
        );
      let w = Resource.fd w in
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

type t = Process : ('a * ('a, [> `Generic | `Unix ], _) Eio.Process.process_ty) -> t [@@unboxed]
type process = t

module Process = struct
  let to_generic (Process p) = Eio.Process.Process p
end

module type MGR_unix = sig
  include Eio.Process.MGR

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

type ('t, 'tag, 'row) mgr_ty =
  < mgr : (module Eio.Process.MGR with type t = 't and type tag = 'tag)
  ; mgr_unix :  (module MGR_unix with type t = 't and type tag = 'tag)
  ; .. > as 'row

type mgr = Mgr : ('a * ('a, [> `Generic | `Unix ], _) mgr_ty) -> mgr [@@unboxed]

module Mgr = struct
  let to_generic (Mgr (a, ops)) = Eio.Process.Mgr (a, ops)
end

module Pi = struct
  let mgr_unix (type t tag) (module X : MGR_unix with type t = t and type tag = tag) =
    let o = Eio.Process.Pi.mgr (module X) in
    object method mgr = o#mgr method mgr_unix = (module X : MGR_unix with type t = t and type tag = tag) end
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

  type tag = [ `Generic | `Unix ]

  let pipe _ ~sw =
    (Private.pipe sw :> ([Eio.Resource.close_ty | Eio.Flow.source_ty] r *
    [Eio.Resource.close_ty | Eio.Flow.sink_ty] r))

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
    X.spawn_unix v ~sw ?cwd ~env ~fds ~executable args |> Process.to_generic

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
