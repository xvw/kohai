(** Pretty Printers to display a standard representation of the data
    described with Rensai. This is probably only for debugging
    purposes. *)

(** A shortcut describing a Rensai expression pretty-printer. *)
type ast = Format.formatter -> Ast.t -> unit

(** A shortcut describing a Rensai Kind of expression
    pretty-printer. *)
type kind = Format.formatter -> Kind.t -> unit

(** A shortcut describing a value error. *)
type value_error = Format.formatter -> Validation.value_error -> unit

(** A pretty-printer for Rensai expressions. *)
val pp_ast : ast

(** A pretty-printer for Rensai Kind of expressions. *)
val pp_kind : kind

(** A pretty-printer for value error. *)
val pp_value_error : value_error
