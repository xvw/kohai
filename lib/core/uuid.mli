(** Uuid5 for cross-reference indexing. *)

type t

(** Generate a UUID for a given string. *)
val gen : string -> t

val to_rensai : t Rensai.Ast.conv
val from_rensai : t Rensai.Validation.t
val to_string : t -> string

(** {1 Set} *)

module Set : sig
  type uid := t
  type t

  val from_list : Rensai.Ast.t list -> t
  val to_list : t -> uid list
  val dump : t -> string
  val to_rensai : t Rensai.Ast.conv
  val from_rensai : t Rensai.Validation.t
  val push : uid -> t -> t
  val remove : uid -> t -> t
end
