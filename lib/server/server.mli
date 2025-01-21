(** A simple server to handle Kohai JSONRpc method. *)

val run
  :  Eff.handler
  -> ?backlog:int
  -> ?reuse_addr:bool
  -> ?reuse_port:bool
  -> ?log_level:Logs.level
  -> port:int
  -> Eio_unix.Stdenv.base
  -> unit
