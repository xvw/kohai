(** [Lang] describes a small syntax used as a substitute for JSON
    (because it's fun to reinvent the wheel). *)

(** Lift a string into a Rensai expression. *)
val from_string : string -> Ast.t option

(** Pretty-printer according to the visual representation of a
    Rensai.Lang. Something printer with the following pretty-printer
    should be bi-directionnaly parsed.*)
val pp : Format.formatter -> Ast.t -> unit
