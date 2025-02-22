(** Represents a URL. *)

type t

val to_uri : t -> Uri.t
val from_string : (string, t) Rensai.Validation.v
val from_rensai : t Rensai.Validation.t
val to_rensai : t Rensai.Ast.conv
val to_compact_rensai : t Rensai.Ast.conv
