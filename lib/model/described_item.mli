(** Describe an item associated to a description (the item name is
    always unique in a set). Something that can be described as a
    category for log or a project. *)

type t

(** Convert item to rensai lang. *)
val to_rensai : t Rensai.Ast.conv

(** Convert rensai expression to item. *)
val from_rensai : t Rensai.Validation.t

val make : ?counter:int -> ?description:string -> string -> t
val can_be_erased : t -> bool
val name : t -> string
val description : t -> string option
val counter : t -> int

module Set : sig
  type item := t
  type t

  (** [push item items] push a item in the item list, if the
      item already exists, it takes the most complete.*)
  val push : item -> t -> t

  val from_ast_list : Rensai.Ast.t list -> t
  val from_list : item list -> t
  val empty : t
  val to_list : t -> item list

  (** Convert item set to rensai lang. *)
  val to_rensai : t Rensai.Ast.conv

  (** Convert rensai expression to item set. *)
  val from_rensai : t Rensai.Validation.t

  (** Render a item set into a string to be stored in a file. *)
  val dump : t -> string

  (** find a item in a set. *)
  val find : string -> t -> item option

  (** Remove an item from the given set.*)
  val remove : string -> t -> t

  (** Increase an item counter. *)
  val increase : string -> t -> t

  (** Decrease an item counter. *)
  val decrease : string -> t -> t
end
