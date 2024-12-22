type ('t, 'module_type) ext = ..

module Type_eq_opt = struct
  type (_, _) t =
    | Equal : ('a, 'a) t
    | Not_equal : ('a, 'b) t
end

type ('t, 'module_type) key =
  { ext : ('t, 'module_type) ext
  ; same_witness : 'm2. ('t, 'm2) ext -> ('module_type, 'm2) Type_eq_opt.t
  }

let same_witness : ('t, 'mt1) key -> ('t, 'mt2) key -> ('mt1, 'mt2) Type_eq_opt.t =
  fun t1 t2 -> t1.same_witness t2.ext
;;

module Key (X : sig
    type 'a t
  end) = struct
  type (_, _) ext += T : ('a, 'a X.t) ext

  let same_witness (type a m2) t2 : (a X.t, m2) Type_eq_opt.t =
    match (t2 : (a, m2) ext) with
    | T -> Type_eq_opt.Equal
    | _ -> Not_equal
  ;;

  let key = { ext = T; same_witness }
end

type ('a, 'data) data_binding = { key : ('a, 'data) key ; mutable data : 'data }
type 'a binding = B : ('a, 'data) data_binding -> 'a binding
type 'a t = 'a binding list ref

let create () = ref []

let find_a_binding : type a data. a t -> key:(a, data) key -> (a, data) data_binding option =
  fun t ~key ->
  let rec aux : a binding list -> (a, data) data_binding option = function
    | [] -> None
    | B b :: tl ->
      match same_witness key b.key with
      | Equal -> Some b
      | Not_equal -> aux tl
  in
  aux !t
;;

let set (t : _ t) ~key ~data =
  match find_a_binding t ~key with
  | Some a -> a.data <- data
  | None -> t := B { key; data } :: !t
;;

let find : type a data. a t -> key:(a, data) key -> data option = fun t ~key ->
  match find_a_binding t ~key with
  | None -> None
  | Some b -> Some b.data
;;

type 'b accessor = { key : 'a. ('a, 'a -> 'b) key }
