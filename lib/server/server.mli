(** A simple server to handle Kohai JSONRpc method. *)

val run : Eff.handler -> Eio_unix.Stdenv.base -> unit
