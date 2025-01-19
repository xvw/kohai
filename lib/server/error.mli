(** Error definitions (mostly related to Jsonrpc). *)

type t

val to_rensai : t -> Rensai.Ast.t
val parse_error : unit -> t

val invalid_request
  :  ?data:Rensai.Validation.value_error
  -> ?id:int
  -> unit
  -> t

val method_not_found : ?data:string -> ?id:int -> unit -> t
val invalid_params : ?data:Rensai.Validation.value_error -> ?id:int -> unit -> t
val internal_error : ?id:int -> unit -> t
val no_supervised_directory : ?id:int -> unit -> t
val custom_error : ?id:int -> code:int -> message:string -> unit -> t
