(** Pretty Printers to display a standard representation of the data
    described with Rensai. This is probably only for debugging
    purposes. *)

(** A shortcut describing a Rensai expression pretty-printer. *)
type t = Format.formatter -> Rensai.Ast.t -> unit

(** A pretty-printer for Rensai expressions. *)
val pp : t
