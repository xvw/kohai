(** [Lang] describes a small syntax used as a substitute for JSON
    (because it's fun to reinvent the wheel). *)

(** Lift a string into a Rensai expression. *)
val from_string : string -> Ast.t option

(** Read a lexingbuf into a Rensai expression. *)
val from_lexingbuf : Lexing.lexbuf -> Ast.t option

(** Read a lexingbuf into a Rensai expression (or null if there is no valid expression).
*)
val from_lexingbuf_or_null : Lexing.lexbuf -> Ast.t

(** Lex the content of a lexing buf and collect every result in a
    string. By default, the result is not reversed. Use [?reverse] to
    trigger [List.rev]. *)
val from_lexingbuf_to_list : ?reverse:bool -> Lexing.lexbuf -> Ast.t list

(** Pretty-printer according to the visual representation of a
    Rensai.Lang. Something printer with the following pretty-printer
    should be bi-directionnaly parsed.*)
val pp : Format.formatter -> Ast.t -> unit

(** Dump an element into a string. *)
val dump : ('a -> Ast.t) -> 'a -> string

(** Dump a list of element into a lexbuffable string. *)
val dump_list : ('a -> Ast.t) -> 'a list -> string
