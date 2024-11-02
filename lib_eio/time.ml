open Std

exception Timeout

type 'a clock_ty = [`Clock of 'a]
type 'a clock_base = 'a r constraint 'a = [> _ clock_ty]

module Pi = struct
  module type CLOCK = sig
    type t
    type time
    val now : t -> time
    val sleep_until : t -> time -> unit
  end

  module Make (X : sig type time end) = struct

    module Clock : sig
      val pi : ('t, (module CLOCK with type t = 't and type time = X.time), [> X.time clock_ty]) Resource.pi
    end = Resource.Pi.Create (struct
        type 'a iface = (module CLOCK with type t = 'a and type time = X.time)
      end)

    let clock (type t) (module X : CLOCK with type t = t and type time = X.time) =
      Resource.handler [ H (Clock.pi, (module X)) ]

    let now (t : [> X.time clock_ty] r) =
      let Resource.T (t, ops) = t in
      let module X = (val (Resource.get ops Clock.pi)) in
      X.now t

    let sleep_until (t : [> X.time clock_ty] r) time =
      let Resource.T (t, ops) = t in
      let module X = (val (Resource.get ops Clock.pi)) in
      X.sleep_until t time
  end

  module Float = Make (struct type time = float end)
  module Mtime = Make (struct type time = Mtime.t end)
end

type 'a clock = ([> float clock_ty] as 'a) r

let now = Pi.Float.now
let sleep_until = Pi.Float.sleep_until

let sleep t d = sleep_until t (now t +. d)

module Mono = struct
  type ty = Mtime.t clock_ty
  type 'a t = ([> ty] as 'a) r

  let now = Pi.Mtime.now
  let sleep_until = Pi.Mtime.sleep_until

  let sleep_span t span =
    match Mtime.add_span (now t) span with
    | Some time -> sleep_until t time
    | None -> Fiber.await_cancel ()

  (* Converting floats via int64 is tricky when things overflow or go negative.
     Since we don't need to wait for more than 100 years, limit it to this: *)
  let too_many_ns = 0x8000000000000000.

  let span_of_s s =
    if s >= 0.0 then (
      let ns = s *. 1e9 in
      if ns >= too_many_ns then Mtime.Span.max_span
      else Mtime.Span.of_uint64_ns (Int64.of_float ns)
    ) else Mtime.Span.zero      (* Also happens for NaN and negative infinity *)

  let sleep t s =
    sleep_span t (span_of_s s)
end

let with_timeout t d = Fiber.first (fun () -> sleep t d; Error `Timeout)
let with_timeout_exn t d = Fiber.first (fun () -> sleep t d; raise Timeout)

module Timeout = struct
  type t =
    | Timeout of Mono.ty r * Mtime.Span.t
    | Unlimited

  let none = Unlimited
  let v clock time = Timeout ((clock :> Mono.ty r), time)

  let seconds clock time =
    v clock (Mono.span_of_s time)

  let run t fn =
    match t with
    | Unlimited -> fn ()
    | Timeout (clock, d) ->
      Fiber.first (fun () -> Mono.sleep_span clock d; Error `Timeout) fn

  let run_exn t fn =
    match t with
    | Unlimited -> fn ()
    | Timeout (clock, d) ->
      Fiber.first (fun () -> Mono.sleep_span clock d; raise Timeout) fn

  let sleep t =
    match t with
    | Unlimited -> Fiber.await_cancel ()
    | Timeout (clock, d) -> Mono.sleep_span clock d

  let pp_duration f d =
    if d >= 0.001 && d < 0.1 then
      Fmt.pf f "%.2gms" (d *. 1000.)
    else if d < 120. then
      Fmt.pf f "%.2gs" d
    else
      Fmt.pf f "%.2gm" (d /. 60.)

  let pp f = function
    | Unlimited -> Fmt.string f "(no timeout)"
    | Timeout (_clock, d) ->
      let d = Mtime.Span.to_float_ns d /. 1e9 in
      pp_duration f d
end
