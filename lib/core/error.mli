(** Description of errors propagated by a JSONRPC server. *)

(** {1 Types} *)

(** Jsonrpc errors. *)
type t

(** Custom errors. *)
type custom

(** {1 Build errors} *)

(** {2 Jsonrpc errors} *)

val parse_error : body:string -> unit -> t

val invalid_request
  :  body:string
  -> error:Rensai.Validation.value_error
  -> unit
  -> t

val method_not_found : body:string -> ?id:int -> meth:string -> unit -> t

val invalid_params
  :  body:string
  -> ?id:int
  -> error:Rensai.Validation.value_error
  -> unit
  -> t

val internal_error : body:string -> ?id:int -> message:string -> unit -> t

val custom_error
  :  ?with_offset:bool
  -> body:string
  -> ?id:int
  -> ?code:int
  -> ?message:string
  -> unit
  -> t

(** {2 Internal errors} *)

val custom_to_jsonrpc : body:string -> ?id:int -> custom -> t
val unknown_error : message:string -> unit -> custom
val no_supervised_directory : unit -> custom
val supervised_directory_error : message:string -> unit -> custom

val resource_not_found
  :  index:string
  -> subject:string
  -> code:int
  -> unit
  -> custom

val no_related_transient_log : index:int -> unit -> custom
val no_related_log : uuid:Uuid.t -> unit -> custom
val invalid_datetime : float -> Rensai.Validation.value_error -> custom

(** {1 Rensai} *)

val to_rensai : t -> Rensai.Ast.t
