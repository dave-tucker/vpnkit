(** HTTP Proxy *)

module Make
    (S: Cohttp_lwt.Server)
  : sig
    type s = Conduit_mirage.server -> S.t -> unit Lwt.t
    (** The type for HTTP Callbacks *)

    val start: s -> unit Lwt.t
    (** The HTTP Proxy Start function *)
  end
