(** A simple server to handle Kohai JSONRpc method. *)

val input_parser : string Eio.Buf_read.parser
val run : Eff.handler -> Eio_unix.Stdenv.base -> unit
