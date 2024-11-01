module Type_eq_opt = struct
  type (_, _) t =
    | Equal : ('a, 'a) t
    | Not_equal : ('a, 'b) t
end

type ('t, 'impl) ext = ..

type ('t, 'impl, 'tags) pi =
  { ext : ('t, 'impl) ext
  ; same_witness : 'm2. ('t, 'm2) ext -> ('impl, 'm2) Type_eq_opt.t
  }

let same_witness : type t i1 i2. (t, i1, _) pi -> (t, i2, _) pi -> (i1, i2) Type_eq_opt.t =
  fun t1 t2 -> t1.same_witness t2.ext
;;

module Pi = struct
  module Create1 (X : sig
      type (!'a, 'b) t
      type 'a iface
    end) =
  struct
    type (_, _) ext += T : (('a, 'b) X.t, 'a X.iface) ext

    let same_witness (type a m2) t2 : (a X.iface, m2) Type_eq_opt.t =
      match (t2 : ((a, _) X.t, m2) ext) with
      | T -> Type_eq_opt.Equal
      | _ -> Not_equal
    ;;

    let pi = { ext = T; same_witness }
  end

  module Create2 (X : sig
      type (!'a, !'b, 'c) t
      type ('a, 'b) iface
    end) =
  struct
    type (_, _) ext += T : (('a, 'b, 'c) X.t, ('a, 'b) X.iface) ext

    let same_witness (type a b m2) t2 : ((a, b) X.iface, m2) Type_eq_opt.t =
      match (t2 : ((a, b, _) X.t, m2) ext) with
      | T -> Type_eq_opt.Equal
      | _ -> Not_equal
    ;;

    let pi = { ext = T; same_witness }
  end

  module Create (X : sig
      type 'a iface
    end) =
    Create1 (struct
      type (!'a, _) t = 'a
      type 'a iface = 'a X.iface
    end)
end

type _ binding = H : ('t, 'impl, 'tags) pi * 'impl -> 't binding
type 't ops = 't binding array
type ('t, 'tags) handler = 't ops
type -'a t = T : ('t * 't ops) -> 'a t

let not_supported () = failwith "Operation not supported!"

let handler = Array.of_list
let bindings = Array.to_list

let get : type t impl tags. t ops -> (t, impl, tags) pi -> impl = fun ops op ->
  let rec aux i : impl =
    if i = Array.length ops then not_supported ();
    let H (k, v) = ops.(i) in
    match same_witness k op with
    | Equal -> v
    | Not_equal -> aux (i + 1)
  in
  aux 0

let get_opt : type t impl tags. t ops -> (t, impl, tags) pi -> impl option = fun ops op ->
  let rec aux i : impl option =
    if i = Array.length ops then None
    else (
      let H (k, v) = ops.(i) in
      match same_witness k op with
      | Equal -> Some v
      | Not_equal -> aux (i + 1)
    )
  in
  aux 0

type close_ty = [`Close]

module Close : sig
  val pi : ('t, 't -> unit, [> close_ty]) pi
end = Pi.Create (struct type 't iface = 't -> unit end)

let close (T (t, ops)) = get ops Close.pi t
