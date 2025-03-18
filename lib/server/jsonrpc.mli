(** A very naive approach to describing compliant services/methods
    with JSONRPC 2.0.

    There are already many libraries that do this very well... but as
    mentioned, I really like the principle of reinventing the wheel,
    gradually adding the necessary functionality. *)

(** {1 Services} *)

(** The type that describes a service/method. *)
type service

(** [service ~meth ~with_params ~finalizer callback] describe a pair
    [meth/service]. *)
val service
  :  meth:string
  -> with_params:'a Rensai.Validation.t
  -> finalizer:(Kohai_model.Context.t -> 'b Rensai.Ast.conv)
  -> (Eff.handler -> Kohai_model.Context.t -> 'a -> 'b)
  -> string * service

(** {1 Run} *)

(** [run ~services body (module Handler)] tries to transform the
    request body through the various services described. *)
val run
  :  Eff.handler
  -> services:(string * service) list
  -> string
  -> (Rensai.Ast.t, Error.t) result
