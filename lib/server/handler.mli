(** Every methods handled by the server. *)

type t = string * Jsonrpc.handler

(** {1 Methods} *)

val ping : t
val ensure_supervised_directory : Core.Path.t option ref -> t
val set_supervised_directory : Core.Path.t option ref -> t
