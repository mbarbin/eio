  type 'a t = private string
  (** The raw bytes of the IP address.
      It is either 4 bytes long (for an IPv4 address) or
      16 bytes long (for IPv6). *)

  (** IPv4 addresses. *)
  module V4 : sig
    val any : [> `V4] t
    (** A special IPv4 address, for use only with [listen], representing
        all the Internet addresses that the host machine possesses. *)

    val loopback : [> `V4] t
    (** A special IPv4 address representing the host machine ([127.0.0.1]). *)
  end

  (** IPv6 addresses. *)
  module V6 : sig
    val any : [> `V6] t
    (** A special IPv6 address, for use only with [listen], representing
        all the Internet addresses that the host machine possesses. *)

    val loopback : [> `V6] t
    (** A special IPv6 address representing the host machine ([::1]). *)
  end

  val pp : [< `V4 | `V6] t Fmt.t
  (** [pp] formats IP addresses.
      For IPv6 addresses, it follows {{:http://tools.ietf.org/html/rfc5952}}. *)

val pp_for_uri : Format.formatter -> string -> unit

  type v4v6 = [`V4 | `V6] t

  val fold :
    v4:([> `V4] t -> 'a) -> 
    v6:([> `V6] t -> 'a) ->
    [< `V4 | `V6] t ->
    'a
  (** [fold ~v4 ~v6 t] is [v4 t] if [t] is an IPv4 address, or [v6 t] if it's an IPv6 address. *)

  (** {2 Interoperability}

  To convert to or from OCaml Unix addresses, use {!Eio_unix.Ipaddr}.

  To interoperate with the {{:https://opam.ocaml.org/packages/ipaddr/} ipaddr} library:
  - [Ipaddr.to_octets ipaddr_ip |> Eio.Net.Ipaddr.of_raw]
  - [Ipaddr.of_octets_exn (eio_ip :> string)] *)

  val of_raw : string -> v4v6
  (** [of_raw addr] casts [addr] to an IP address.
      @raise Invalid_argument if it is not 4 or 16 bytes long. *)
