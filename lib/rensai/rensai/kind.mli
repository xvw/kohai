(** describes and classifies the different types of nodes in the
    Rensai data AST. *)

(** Set of possible node types. *)
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
  | Pair
  | List
  | Constr
  | Record

(** [classify fragment] returns the main classification of a node. *)
val classify : Ast.t -> t
