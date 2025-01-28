(** Describe a Sector. Something that can be described as a category
    for log. *)

type t

val to_rensai : t Rensai.Ast.conv
val from_rensai : t Rensai.Validation.t
