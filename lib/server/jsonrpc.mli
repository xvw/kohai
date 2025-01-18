(** A very naïve way to deal with JSONRPC. *)

(** {1 Types} *)

(** Type that describes an input. *)
type input

(** Type that describes an error. *)
type error

(** Type that describes a method handler. *)
type handler

(** {1 Build errors} *)

val parse_error : unit -> error

val invalid_request
  :  ?data:Rensai.Validation.value_error
  -> ?id:int
  -> unit
  -> error

val method_not_found : ?id:int -> unit -> error

val invalid_params
  :  ?data:Rensai.Validation.value_error
  -> ?id:int
  -> unit
  -> error

val internal_error : ?id:int -> unit -> error
val no_supervised_directory : ?id:int -> unit -> error
val custom_error : ?id:int -> code:int -> message:string -> unit -> error

(** {1 Build result} *)

val error : error -> Rensai.Ast.t

(** [result ?id f x] lift [f x] into an response. *)
val result
  :  ?id:int
  -> ('a -> Rensai.Ast.t)
  -> 'a
  -> (Rensai.Ast.t, error) result

(** {1 Handlers} *)

val handler
  :  string
  -> params:'a Rensai.Validation.t
  -> (?id:int -> 'a -> (Rensai.Ast.t, error) result)
  -> string * handler

val services
  :  (string * handler) list
  -> string
  -> Http.Status.t * Rensai.Json.yojson
