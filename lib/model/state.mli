(** Describes an application state (essentially for caching). *)

type t

(** Create a new cache environment. *)
val big_bang : unit -> t

(** Update date boundaries of the cache. *)
val patch_date_boundaries : Datetime.t -> t -> t

(** {1 Serialization} *)

val from_rensai : t Rensai.Validation.t
val to_compact_rensai : t Rensai.Ast.conv
val from_string : string -> t
val dump : t -> string
