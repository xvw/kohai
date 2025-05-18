(** Represents a URL. *)

type t
type scheme

val make
  :  uri:Uri.t
  -> scheme:scheme
  -> port:int option
  -> host:string
  -> query:string list Key_value.t
  -> path:Path.t
  -> unit
  -> t

val validate_scheme : string -> scheme
val to_uri : t -> Uri.t
val from_string : (string, t) Rensai.Validation.v
val from_rensai : t Rensai.Validation.t
val to_rensai : t Rensai.Ast.conv
val to_compact_rensai : t Rensai.Ast.conv
