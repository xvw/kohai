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
  :  body:string
  -> ?id:int
  -> code:int
  -> ?message:string
  -> unit
  -> Sigs.jsonrpc_error
