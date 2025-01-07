(** describes and classifies the different types of nodes in the
    Rensai data AST.

    The main purpose of kind is to report errors. So it's not a
    reliable type system at all (as demonstrated by the particularly
    sad treatment of records). *)

(** {1 Types} *)

(** Set of possible node types. And is a clumsy way of expressing the
    ambivalence of the List constructor (which can handle both
    homogeneous and heterogeneous lists). Since records carry no
    structural information... they are treated as equivalent and
    notified by the kind [?record].*)
type t =
  | Null
  | Unit
  | Bool
  | Char
  | Int
  | Int32
  | Int64
  | Float
  | String
  | Pair of t * t
  | List of t
  | Constr of string * t
  | Record
  | Any
  | Or of t * t
  | And of t * t

(** {1 Helpers} *)

(** [classify fragment] returns the main classification of a node. *)
val classify : Ast.t -> t

(** [from_list] Builds a kind list as a succession of Or. If the list
    is empty, the result will be [Any]. *)
val from_list : t list -> t

(** Pretty-Printers for Kind. *)
val pp : Format.formatter -> t -> unit

(** Equality between kinds. *)
val equal : t -> t -> bool

(** {2 Infix}

    Infix operators for easy creation of kinds composition. *)

module Infix : sig
  (** [k1 || k2] is the [Or (k1, k2)]. *)
  val ( || ) : t -> t -> t

  (** [k1 && k2] is the [And (k1, k2)]. *)
  val ( && ) : t -> t -> t
end

include module type of Infix (** @inline *)
