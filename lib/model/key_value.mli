(** Describes a key-value structure for assigning arbitrary values
    indexed by strings. *)

type 'a t

val from_rensai : 'a Rensai.Validation.t -> 'a t Rensai.Validation.t
val to_rensai : 'a Rensai.Ast.conv -> 'a t Rensai.Ast.conv
val empty : unit -> 'a t
val add : string -> 'a -> 'a t -> 'a t
val remove : string -> 'a t -> 'a t
val keys : 'a t -> string list
val from_list : (string * 'a) list -> 'a t
val to_list : 'a t -> (string * 'a) list
val is_empty : 'a t -> bool
