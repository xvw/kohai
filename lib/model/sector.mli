(** Describe a Sector. Something that can be described as a category
    for log. *)

type t

(** Convert sector to rensai lang. *)
val to_rensai : t Rensai.Ast.conv

(** Convert rensai expression to sector. *)
val from_rensai : t Rensai.Validation.t

module Set : sig
  type sector := t
  type t

  (** [push sector sectors] push a sector in the sector list, if the
      sector already exists, it takes the most complete.*)
  val push : sector -> t -> t

  val from_list : Rensai.Ast.t list -> t
  val to_list : t -> sector list

  (** Convert sector set to rensai lang. *)
  val to_rensai : t Rensai.Ast.conv

  (** Convert rensai expression to sector set. *)
  val from_rensai : t Rensai.Validation.t
end
