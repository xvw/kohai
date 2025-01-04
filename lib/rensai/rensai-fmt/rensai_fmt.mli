(** Pretty Printers to display a standard representation of the data
    described with Rensai. This is probably only for debugging
    purposes. *)

(** A shortcut describing a Rensai expression pretty-printer. *)
type ast = Format.formatter -> Rensai.Ast.t -> unit

(** A shortcut describing a Rensai Kind of expression
    pretty-printer. *)
type kind = Format.formatter -> Rensai.Kind.t -> unit

(** A pretty-printer for Rensai expressions. *)
val pp_ast : ast

(** A pretty-printer for Rensai Kind of expressions. *)
val pp_kind : kind
