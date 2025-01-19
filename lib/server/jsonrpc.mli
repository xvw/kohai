(** A very naïve way to deal with JSONRPC. *)

type input
type handler

val handler
  :  meth:string
  -> with_params:'a Rensai.Validation.t
  -> finalizer:('b -> Rensai.Ast.t)
  -> (?id:int -> 'a -> 'b Eff.t)
  -> string * handler

val services
  :  (string * handler) list
  -> string
  -> (Rensai.Ast.t, Error.t) result Eff.t
