(** Basic of effect handling. *)

open Core

(** {1 Effect Definition} *)

type 'a t = 'a IO.t
type _ Effect.t += K_fail_with : Error.t -> 'a Effect.t

(** {1 Effect Performance} *)

(** {2 Error Handling} *)

val from_validated
  :  (Rensai.Validation.value_error -> Error.t)
  -> 'a Rensai.Validation.checked
  -> 'a t

val fail_with : Error.t -> 'a t
val parse_error : unit -> 'a t
val method_not_found : ?id:int -> string -> unit -> 'a t
