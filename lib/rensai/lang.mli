(** [Lang] describes a small syntax used as a substitute for JSON
    (because it's fun to reinvent the wheel). *)

val from_string : string -> Ast.t option
