(** Description of errors propagated by a JSONRPC server. *)

(** {1 Types} *)

type t = Sigs.jsonrpc_error

(** {1 Build errors} *)

val parse_error : body:string -> unit -> Sigs.jsonrpc_error

val invalid_request
  :  body:string
  -> ?id:int
  -> ?error:Rensai.Validation.value_error
  -> unit
  -> Sigs.jsonrpc_error

val method_not_found
  :  body:string
  -> ?id:int
  -> meth:string
  -> unit
  -> Sigs.jsonrpc_error

val invalid_params
  :  body:string
  -> ?id:int
  -> error:Rensai.Validation.value_error
  -> unit
  -> Sigs.jsonrpc_error

val internal_error
  :  body:string
  -> ?id:int
  -> ?message:string
  -> unit
  -> Sigs.jsonrpc_error

val custom_error
  :  ?with_offset:bool
  -> body:string
  -> ?id:int
  -> code:int
  -> ?message:string
  -> unit
  -> Sigs.jsonrpc_error

val no_related_transient_log : body:string -> ?id:int -> int -> t
val no_supervised_directory : body:string -> ?id:int -> unit -> t
val supervised_directory_error : body:string -> ?id:int -> string -> unit -> t
val unknown_error : string -> unit -> t

(** {1 Rensai} *)

val to_rensai : t -> Rensai.Ast.t
