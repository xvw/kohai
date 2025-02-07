(** Describe a Sector. Something that can be described as a category
    for log. *)

type t

(** Convert sector to rensai lang. *)
val to_rensai : t Rensai.Ast.conv

(** Convert rensai expression to sector. *)
val from_rensai : t Rensai.Validation.t

(** [push sectors sector] push a sector in the sector list, if the
    sector already exists, it takes the most complete.*)
val push : t list -> t -> t list
