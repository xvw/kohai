(** Represents a stored log (non-transient). *)

(** Type describing a log. *)
type t

(** Convert a log into a transient one. *)
val from_transient_log : Transient_log.t -> t option

(** Validate a log from a rensai expression. *)
val from_rensai : t Rensai.Validation.t

(** Lift a log into a dumpable rensai expression. *)
val to_rensai : t -> Rensai.Ast.t

(** Properly sort a list of logs. *)
val sort : t list -> t list

(** Read a list of logs from a file content. *)
val from_file_content : string -> t list

(** Find the relevant file for a log. *)
val find_file : cwd:Path.t -> t -> Path.t

(** Render a list of logs into a string to be stored in a file. *)
val dump : t list -> string
